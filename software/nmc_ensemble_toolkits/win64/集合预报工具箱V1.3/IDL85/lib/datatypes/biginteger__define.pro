; $Id: //depot/Release/ENVI53_IDL85/idl/idldir/lib/datatypes/biginteger__define.pro#1 $
;
; Copyright (c) 2013-2015, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.
;

;---------------------------------------------------------------------------
function BigInteger::Init, valueIn, RADIX=radixIn, SIGN=sign
  compile_opt idl2, hidden
  on_error, 2

  ; Happy path - if we have valid input (usually from one of our methods).
  if (ISA(valueIn, 'ULONG')) then begin
    self.pDigits = PTR_NEW(valueIn)
    self.isNegative = ISA(sign) && sign eq -1 && ~ARRAY_EQUAL(valueIn, 0)
    return, 1
  endif

  value = ISA(valueIn) ? valueIn : !NULL
  radix = ISA(radixIn) ? radixIn[0] : 10
  if (radix lt 2 || radix gt 36) then begin
    MESSAGE, 'Radix must be between 2 and 36.'
  endif
  
  if (ISA(value, /NUMBER)) then begin
    if (radix ne 10) then begin
      MESSAGE, 'RADIX can only be used with a string value.'
    endif
    if (N_ELEMENTS(value) gt 1) then begin
      MESSAGE, 'Value must be a ULONG array, a scalar value, or a string.'
    endif
    value = value[0]
    if (value ne 0) then begin
      ; Fast path for small numbers.
      if (ULONG64(ABS(value)) lt 4294967296uLL) then begin
        self.isNegative = (value lt 0) || (ISA(sign) && sign eq -1)
        self.pDigits = PTR_NEW(ULONG(ABS(value)))
      endif else begin
        ; Convert to a string and handle down below.
        ; This makes it easy to convert 64-bit numbers.
        isFloat = ISA(value, 'FLOAT') || ISA(value, 'DOUBLE')
        if (~isFloat || ABS(value) lt 1d18) then begin
          value = STRING(value, FORMAT='(I0)')
        endif else begin
          ; For a float, print out all the digits and tack on zeroes.
          powerOfTen = LONG(ALOG10(value)) - 18
          value = STRING(value/(10d^powerOfTen), FORMAT='(I0)')
          if (powerOfTen gt 0) then begin
            value += STRJOIN(REPLICATE('0', powerOfTen))
          endif
        endelse
      endelse
      void = CHECK_MATH()
    endif
  endif

  if (ISA(value, 'STRING')) then begin
    if (N_ELEMENTS(value) gt 1) then begin
      MESSAGE, 'Value must be a scalar string or number.'
    endif

    val = STRTRIM(value[0],2)

    slen = STRLEN(val)
    val = STRUPCASE(val)
    if (~STREGEX(val, '^[+-]?[0-9A-Z]+$', /BOOLEAN)) then begin
      MESSAGE, 'Unable to convert string to a number: "' + value[0] + '"'
    endif
    bytes = BYTE(val)
    if (bytes[0] eq 43b || bytes[0] eq 45b) then begin
      bytes = bytes[1:*]
    endif
    ; Convert from byte characters for [0-9, A-Z] to numbers 0 - 35
    bytes -= 48
    bytes -= 7*(bytes ge 17)
    if (MIN(bytes, MAX=mx) lt 0 || mx ge radix) then begin
      MESSAGE, 'Out-of-range digit encountered for radix=' + $
        STRTRIM(radix,2) + ': "' + value[0] + '"'
    endif
    digits = 0uL
    foreach b, bytes, i do begin
      digits = BigInteger_Add(BigInteger_Multiply(digits, radix), b)
    endforeach

    self.isNegative = (ISA(sign) && sign eq -1) || STRMID(val,0,1) eq '-'
    self.pDigits = PTR_NEW(digits)
  endif

  return, 1
end


;---------------------------------------------------------------------------
pro BigInteger::GetProperty, DIGITS=digits, SIGN=sign
  compile_opt idl2, hidden, static
  on_error, 2
  ; It is faster to get the properties without checking for arg_present.
  ; Also, we need digits to compute sign.
  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  sign = ARRAY_EQUAL(digits, 0uL) ? 0 : (self.isNegative ? -1 : 1)
end


;---------------------------------------------------------------------------
function BigInteger::_addDigitsWithSigns, arg1, arg2, SUBTRACT=subtract
  compile_opt idl2, hidden
  on_error, 2

  var1 = ISA(arg1, 'BigInteger') ? arg1 : BigInteger(arg1)
  var2 = ISA(arg2, 'BigInteger') ? arg2 : BigInteger(arg2)
  var1.GetProperty, DIGITS=digits1, SIGN=sign1
  var2.GetProperty, DIGITS=digits2, SIGN=sign2
  
  ; Quick return for 0's.
  if (sign1 eq 0) then return, var2
  if (sign2 eq 0) then return, var1

  if (KEYWORD_SET(subtract)) then begin
    sign2 = -sign2
  endif

  n1 = N_ELEMENTS(digits1)
  n2 = N_ELEMENTS(digits2)

  ; Both numbers are negative. Just flip the sign of the positive sum.
  doFlip = (sign1 eq -1) && (sign2 eq -1)

  if (sign1 eq sign2) then begin

    ; Both numbers are either positive or negative
    result = BigInteger_Add(digits1, digits2)

  endif else begin

    ; One of the numbers is negative.
    n = N_ELEMENTS(digits1) > N_ELEMENTS(digits2)
    ; To avoid negative values when subtracting, we find out which number
    ; has the biggest magnitude and subtract the other one from it.
    ; Then we flip the sign of the result if necessary.
    compare = BigInteger_Compare(digits1, digits2)
    if (compare eq 1) then begin
      ; #1 is bigger so flip the sign if #1 was the negative number
      doFlip = sign1 eq -1
      leftNumber = digits1
      rightNumber = digits2
    endif else begin
      ; #2 is bigger so flip the sign if #2 was the negative number
      doFlip = sign2 eq -1
      leftNumber = digits2
      rightNumber = digits1
    endelse

    result = BigInteger_Subtract(leftNumber, rightNumber)

  endelse

  return, BigInteger(result, SIGN=(doFlip ? -1 : 1))
end


;---------------------------------------------------------------------------
function BigInteger::_bitLength, digits
  compile_opt idl2, hidden
  on_error, 2

  powersOfTwo = 2L^(LINDGEN(32))
  setBits = 1 + MAX(WHERE(digits[-1] ge powersOfTwo))
  result = setBits + 32LL*(N_ELEMENTS(digits) - 1)
  return, result
end


;---------------------------------------------------------------------------
function BigInteger::BitLength
  compile_opt idl2, hidden
  on_error, 2

  result = self._bitLength(PTR_VALID(self.pDigits) ? *self.pDigits : 0)
  return, result
end


;---------------------------------------------------------------------------
; Returns -1 if "digits1" is less than "digits2"
; Returns 0 if they are equal
; Returns +1 if "digits1" is greater than "digits2"
;
function BigInteger::_compareTo, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2

  var1 = ISA(arg1, 'BigInteger') ? arg1 : BigInteger(arg1)
  var2 = ISA(arg2, 'BigInteger') ? arg2 : BigInteger(arg2)
  var1.GetProperty, DIGITS=digits1, SIGN=sign1
  var2.GetProperty, DIGITS=digits2, SIGN=sign2

  compare = BigInteger_Compare(digits1, digits2)
  
  ; digits1 has the bigger magnitude
  if (compare eq 1) then begin
    ; If digits1 is actually negative, then it is "less than"
    return, (sign1 eq -1) ? -1 : 1
  endif

  ; digits2 has the bigger magnitude
  if (compare eq -1) then begin
    ; If digits2 is actually negative, then digits1 is "greater than"
    return, (sign2 eq -1) ? 1 : -1
  endif
  
  ; They must be equal.
  return, 0
end


;---------------------------------------------------------------------------
; Recursive routine to convert digits to a different radix.
; Divide the number by our new radix^(n/2). We can then concatenate
; the quotient and remainder to get our string of digits.
; Returns the new digits in little-endian (largest digits at the end).
;
function BigInteger::_Convert, digits, radix
  compile_opt idl2, hidden
  on_error, 2

  n = N_ELEMENTS(digits)
  ; For a small # of digits, just convert directly.
  ; This also stops our recursion.
  if (n le 100 || ARRAY_EQUAL(digits, 0uL)) then begin
    return, BigInteger_Convert(digits, radix)
  endif

  ; Try to split the number into two equal numbers of digits.
  ; Take n/2 and then multiply by the ratio of the log of the radixes.
  nsplit = CEIL((n/2)*32/ALOG2(radix)) > 1
  radixDigits = self._Exponent(radix, nsplit)
  upper = BigInteger_Divide(digits, radixDigits, lower)

  diff = N_ELEMENTS(radixDigits) - N_ELEMENTS(lower)

  ; If our remainder is 0, we won't have any idea how many actual 0 digits
  ; we need. So give up and use the slow approach for this pass.
  if (diff ne 0 || ARRAY_EQUAL(lower, 0uL)) then begin
    return, BigInteger_Convert(digits, radix)
  endif

  ; Now recurse, splitting the lower and upper parts in half again.
  lower = self._Convert(lower, radix)
  upper = self._Convert(upper, radix)

  ; Since we divided by a power of our new radix, we can just concatenate.
  return, [lower, upper]
end


;---------------------------------------------------------------------------
function BigInteger::_Divide, arg2, REMAINDER=remainder
  compile_opt idl2, hidden
  on_error, 2

  digits1 = PTR_VALID(self.pDigits) ? *self.pDigits : 0
  neg1 = self.isNegative
  var2 = ISA(arg2, 'BigInteger') ? arg2 : BigInteger(arg2)
  var2.GetProperty, DIGITS=digits2, SIGN=sign2
  neg2 = (sign2 eq -1)

  quotient = BigInteger_Divide(digits1, digits2, remainderDigits)

  ; If one of the operands is negative, then the sign is negative.
  sign = (neg1 ne neg2) ? -1 : 1
  result = BigInteger(quotient, SIGN=sign)

  if (ARG_PRESENT(remainder)) then begin
    ; The sign for the remainder only depends upon the first operand.
    sign = (neg1 ? -1 : 1)
    remainder = BigInteger(remainderDigits, SIGN=sign)
  endif

  return, result
end


;---------------------------------------------------------------------------
function BigInteger::_Exponent, digits, exponent
  compile_opt idl2, hidden

  if (exponent ge 2) then begin
    ; Split the exponent into halves. Each half will produce the same
    ; answer, so just multiply the answer by itself. If the exponent is odd,
    ; we just multiply the result by digits to pick up the last exponent.
    mLeft = exponent/2
    mRight = exponent - mLeft
    ; This is a recursive call.
    left = self._Exponent(digits, mLeft)
    result = BigInteger_Multiply(left, left)
    if (mLeft ne mRight) then begin
      result = BigInteger_Multiply(result, digits)
    endif
    return, result
  endif

  if (exponent eq 1) then begin
    return, digits
  endif

  if (exponent eq 0) then begin
    return, 1L
  endif
end


;---------------------------------------------------------------------------
function BigInteger::GetPrimes, nIn, COUNT=count, IS_INDEX=isIndexIn
  compile_opt hidden, static
  on_error, 2

  n = ISA(nIn) ? nIn[0] : 1000
  isIndex = ISA(nIn) ? KEYWORD_SET(isIndexIn) : 0b
  if (isIndex) then begin
    if (n lt 1) then MESSAGE, 'Index must be >= 1.'
  endif else begin
    if (n lt 2) then MESSAGE, 'N must be >= 2.
  endelse

  primes = [2uL, 3, 5, 7,11,13,17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, $
     67, 71, 73, 79, 83, 89, 97,101,103,107,109,113,127,131,137,139,149,151, $
    157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251, $
    257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359, $
    367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463, $
    467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593, $
    599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701, $
    709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827, $
    829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953, $
    967,971,977,983,991,997]

  if (n le (isIndex ? 168 : 1000)) then begin
    primes = isIndex ? primes[0:n-1] : primes[WHERE(primes le n, /NULL)]
    count = N_ELEMENTS(primes)
    return, primes
  endif

  primes = [primes, $
  1009,1013,1019,1021,1031,1033,1039,1049,1051,1061,1063,1069,1087,1091,1093, $
  1097,1103,1109,1117,1123,1129,1151,1153,1163,1171,1181,1187,1193,1201,1213, $
  1217,1223,1229,1231,1237,1249,1259,1277,1279,1283,1289,1291,1297,1301,1303, $
  1307,1319,1321,1327,1361,1367,1373,1381,1399,1409,1423,1427,1429,1433,1439, $
  1447,1451,1453,1459,1471,1481,1483,1487,1489,1493,1499,1511,1523,1531,1543, $
  1549,1553,1559,1567,1571,1579,1583,1597,1601,1607,1609,1613,1619,1621,1627, $
  1637,1657,1663,1667,1669,1693,1697,1699,1709,1721,1723,1733,1741,1747,1753, $
  1759,1777,1783,1787,1789,1801,1811,1823,1831,1847,1861,1867,1871,1873,1877, $
  1879,1889,1901,1907,1913,1931,1933,1949,1951,1973,1979,1987,1993,1997,1999]

  if (n le (isIndex ? 303 : 2000)) then begin
    primes = isIndex ? primes[0:n-1] : primes[WHERE(primes le n, /NULL)]
    count = N_ELEMENTS(primes)
    return, primes
  endif

  count = N_ELEMENTS(primes)
  nextprimes = LIST()
  nextprime = primes[-1]
  this = BigInteger(nextprime)

  while (1) do begin
    if (isIndex && count ge n) then break
    nextprime = this._NextPrime(nextprime)
    if (~isIndex && nextprime gt n) then break
    nextprimes.Add, nextprime
    count++
  endwhile

  result = [primes, nextprimes.ToArray(TYPE='ULONG')]
  return, result
end


;---------------------------------------------------------------------------
; Knuth, 4.5.4, Algorithm A (Factoring by division)
; 
; Returns an array of unsigned 32-bit integers containing the prime factors.
; If the number is prime, or only contains factors that are larger than
; the value 1999, then a scalar 1 is returned.
; 
; BIG_FACTOR: returns either a scalar 1 if the prime factorization was
; successful, or returns a BigInteger containing the "large" piece
; that was not factorized. This large factor may be prime or composite,
; but is guaranteed to not have any prime factors <= 1999.
;
function BigInteger::_FactorByDivision, BIG_FACTOR=bigFactor
  compile_opt idl2, hidden
  on_error, 2

  bigFactor = 1uL
  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0
  if (N_ELEMENTS(digits) eq 1 && digits le 1) then begin
    return, 1uL
  endif

  primes = self.GetPrimes(2000)

  ; Sanity check: Make sure we aren't a small prime.
  if (N_ELEMENTS(digits) eq 1 && digits le primes[-1]) then begin
    if (MAX(digits eq primes) eq 1) then begin
      bigFactor = digits
      ; This is a prime number.
      return, 1uL
    endif
  endif

  nCurrent = digits
  pfactors = LIST()
  k = 0

  while (1) do begin
    ; step A2
    if (N_ELEMENTS(nCurrent) eq 1 && nCurrent le 1) then begin
      break
    endif
    ; step A3
    quotient = BigInteger_Divide(nCurrent, primes[k], remainder)
    ; step A4, found a factor?
    if (ARRAY_EQUAL(remainder, 0)) then begin
      ; step A5, yes, this is a factor
      pfactors.Add, primes[k]
      nCurrent = quotient
    endif else begin
      if (BigInteger_Compare(quotient, primes[k]) eq 1) then begin
        ; step A6, quotient is still big, keep going
        k++
        if (k eq N_ELEMENTS(primes)) then begin
          ; We ran out of primes.
          break
        endif
      endif else begin
        ; step A7, quotient is too small, nCurrent must be prime
        pfactors.Add, nCurrent
        nCurrent = 1uL
        break
      endelse
    endelse
  endwhile

  if (ARG_PRESENT(bigFactor)) then begin
    ; If our remaining factor is 1 then we don't have a "big factor".
    ; Otherwise, return a BigInteger.
    bigFactor = (BigInteger_Compare(nCurrent, 1) eq 1) ? $
      BigInteger(nCurrent) : nCurrent
  endif

  return, (N_ELEMENTS(pfactors) gt 0) ? pfactors.toArray() : 1uL
end


;---------------------------------------------------------------------------
; Given an odd BigInteger N, determine the largest factor <= sqrt(N)
; Knuth, 4.5.4, Algorithm C (Factoring by addition and subtraction)
;
function BigInteger::_FactorFermat, NITER=iter
  compile_opt idl2, hidden
  on_error, 2
  
  if (~PTR_VALID(self.pDigits)) then begin
    return, 1
  endif

  sqr = self.Sqrt()
  xx = (2*sqr + 1).digits
  yy = 1
  rem = sqr*sqr - BigInteger(*self.pDigits)
  isNeg = rem.sign eq -1
  rem = rem.digits
  iter = 0uL
  factor = -1
  
  while (1) do begin

    if (ARRAY_EQUAL(rem, 0)) then begin
      factor = BigInteger_Divide(BigInteger_Subtract(xx, yy), 2)
      break
    endif
    
    if (isNeg) then begin
      if (BigInteger_Compare(rem, xx) eq 1) then begin
        ; Still negative
        rem = BigInteger_Subtract(rem, xx)
      endif else begin
        isNeg = 0b
        rem = BigInteger_Subtract(xx, rem)
      endelse
    endif else begin
      rem = BigInteger_Add(rem, xx)
    endelse
    xx = BigInteger_Add(xx, 2)
    iter++

    while (1) do begin
      if (BigInteger_Compare(rem, yy) eq 1) then begin
        isNeg = 0b
        rem = BigInteger_Subtract(rem, yy)
      endif else begin
        isNeg = 1b
        rem = BigInteger_Subtract(yy, rem)
      endelse
      yy = BigInteger_Add(yy, 2)
      iter++
      if (iter gt 1000000) then break
      if (isNeg) then break
    endwhile
    
    if (iter gt 1000000) then break
  endwhile
  return, factor
end


;---------------------------------------------------------------------------
; Return the prime factors.
; Knuth, 4.5.4, Algorithm B (Factoring by the rho method)
;
function BigInteger::_FactorRho, NITER=iter
  compile_opt idl2, hidden
  on_error, 2

  iter = 0uL
  factors = self._FactorByDivision(BIG_FACTOR=bigFactor)
  nsmall = TOTAL(factors gt 1, /INTEGER)
  result = (nsmall gt 1) ? OBJARR(nsmall) : OBJ_NEW()
  for i=0,nsmall-1 do begin
    result[i] = BigInteger(factors[i])
  endfor

  ; Did we successfully find all factors?
  if (bigFactor eq 1) then begin
    return, result
  endif

  pfactors = (nsmall gt 0) ? LIST(result, /EXTRACT) : LIST()

  ; Knuth step B1
  xx = 5
  xp = 2
  k = 1
  l = 1
  ; Start out with all of the leftover huge factors.
  digits = bigFactor.digits
  iter = 0uL

  while (1) do begin

    ; Knuth step B2
    if (self._IsPrimeMillerRabin(digits) && $
      self._IsPrimeLucas(digits)) then begin
      pfactors.Add, BigInteger(digits)
      break
    endif

    ; Knuth step B3
    if (BigInteger_Compare(xp, xx) ge 0) then begin
      tmp = BigInteger_Subtract(xp, xx)
    endif else begin
      tmp = BigInteger_Subtract(xx, xp)
    endelse

    g = self._GCD(tmp, digits)

    if (BigInteger_Compare(g, digits) eq 0) then begin
      MESSAGE, 'RHO method failed.'
    endif

    if (BigInteger_Compare(g, 1) eq 0) then begin
      ; Knuth step B4, advance the algorithm.
      k = BigInteger_Subtract(k, 1)
      if (BigInteger_Compare(k, 0) eq 0) then begin
        xp = xx
        l = BigInteger_Multiply(l, 2)
        k = l
      endif
      ; x = (x^2 + 1) mod n
      xx = BigInteger_Add(BigInteger_Multiply(xx, xx), 1)
      !null = BigInteger_Divide(xx, digits, xx)
    endif else begin
      pfactors.Add, BigInteger(g)
      ; n = n/g, x = x mod n, xp = xp mod n
      digits = BigInteger_Divide(digits, g)
      !null = BigInteger_Divide(xx, digits, xx)
      !null = BigInteger_Divide(xp, digits, xp)
    endelse
    iter++
    if (iter gt 1000000) then begin
      MESSAGE, 'RHO method was stopped after 1000000 iterations.'
    endif
  endwhile

  n = N_ELEMENTS(pfactors)
  return, (n gt 1) ? pfactors.toArray() : (n eq 1 ? pfactors[0] : BigInteger(1))
end


;---------------------------------------------------------------------------
; Recursive algorithm for computing the partial factorial:
;     n2*(n2-1)*(n2-2)*...*(n1+1)*n1
; All factors of two are "discarded" and accumulated in the
; powersOfTwo output argument.
;
function BigInteger::_FactorialPartial, n1, n2, powersOfTwo
  compile_opt idl2, hidden
  on_error, 2

  nNum = n2 - n1 + 1
  
  ; If we have less than 30 numbers, just do the multiplication and return.
  ; This was determined empirically to be the best tradeoff for speed
  ; versus the cost of recursion.
  if (nNum lt 30) then begin
    maxP = CEIL(ALOG2(n2)) < 31
    twos = 2uL^(INDGEN(maxP)+1)
    result = 1uL
    for i=n1, n2 do begin
      ; Collect and discard all powers of two, but remember them!
      p2 = TOTAL((i mod twos) eq 0, /INT)
      powersOfTwo += p2
      result = BigInteger_Multiply(result, i/(2uL^p2))
    endfor
    return, result
  endif

  ; Split the factorial in half and compute recursively.
  a1 = self._FactorialPartial(n1, n1 + nNum/2 - 1, powersOfTwo)
  a2 = self._FactorialPartial(n1 + nNum/2, n2, powersOfTwo)
  ; Multiply the two halves back together.
  result = BigInteger_Multiply(a1, a2)
  return, result
end


;---------------------------------------------------------------------------
; Uses a recursive algorithm where the factorial is subdivided into equal
; halves, and then the two halves are multiplied together at the end.
; With a naive algorithm, just doing n*(n-1)*(n-2)..., you end up multiplying
; a larger and larger number by a single digit. Our BigInteger_Multiply
; routine is much more efficient at multiplying two digits of equal length.
; In addition, with the naive algorithm, the number of digits that need
; to be multiplied increases rapidly. For example, for Factorial(30000),
; just doing a simple loop, 179 million digits end up being multiplied
; together. Using the recursive algorithm, you only multiply 306000 digits,
; although because these are all multi-digit numbers, it only ends up
; being about 3 times faster. For Factorial(40000) it is about 4 times faster.
; For Factorial(100,000) it is 7 times faster.
;
function BigInteger::Factorial, n
  compile_opt idl2, hidden, static
  on_error, 2

  if (n lt 0) then begin
    MESSAGE, 'N must be greater than or equal to 0.'
  endif
  if (n le 1) then begin
    return, BigInteger(1)
  endif

  ; We will not include any powers of two when computing the factorial.
  ; Keep track of them here.
  powersOfTwo = 0uL

  that = BigInteger(1)
  result = that._FactorialPartial(1, n, powersOfTwo)

  ; Now multiply by all of our missing powers of two.
  extraTwos = powersOfTwo mod 32
  if (extraTwos gt 0) then begin
    result = BigInteger_Multiply(result, 2uL^extraTwos)
  endif

  ; For sets of 32 powers-of-two, just shift left by 0ul's.
  setsOfTwos = powersOfTwo/32
  if (setsOfTwos gt 0) then begin
    result = [ULONARR(powersOfTwo/32), result]
  endif

  result = BigInteger(result)
  return, result
end


;---------------------------------------------------------------------------
; Generate the Fermat number, F(n) = 2^(2^n) + 1
;
function BigInteger::Fermat, n
  compile_opt idl2, hidden, static
  on_error, 2

  return, BigInteger(2)^(2uLL^n) + 1
end


;---------------------------------------------------------------------------
; Returns the greatest common divisor (GCD) of self and the input argument.
; Uses Euclid's algorithm, using the least absolute remainder, which is
; guaranteed to have the fewest number of steps.
; This could be improved further by using Euclid's algorithm for large numbers,
; described in Knuth (3rd ed), section 4.5.2, Algorithm L.
;
function BigInteger::_GCD, uNumIn, vNumIn
  compile_opt idl2, hidden
  on_error, 2

  uNum = uNumIn
  vNum = vNumIn

  while (N_ELEMENTS(vNum) gt 1 || vNum[0] ne 0) do begin
    !null = BigInteger_Divide(uNum, vNum, remainder)
    uNum = vNum
    ; Compute both (u mod v) and (v - u mod v)
    ; Keep whichever is smaller for the new "v"
    ; This can significantly reduce the # of steps required.
    negRemainder = BigInteger_Subtract(vNum, remainder)
    negIsSmaller = (BigInteger_Compare(negRemainder, remainder) lt 0)
    vNum = negIsSmaller ? negRemainder : remainder
  endwhile

  return, uNum
end


;---------------------------------------------------------------------------
; Returns the greatest common divisor (GCD) of self and the input argument.
; Uses Euclid's algorithm, using the least absolute remainder, which is
; guaranteed to have the fewest number of steps.
; This could be improved further by using Euclid's algorithm for large numbers,
; described in Knuth (3rd ed), section 4.5.2, Algorithm L.
;
function BigInteger::GCD, vNumIn
  compile_opt idl2, hidden
  on_error, 2

  vNum = ISA(vNumIn, 'BigInteger') ? vNumIn : BigInteger(vNumIn)
  vNum = vNum.digits
  uNum = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  uNum = self._GCD(uNum, vNum)
  return, BigInteger(uNum)
end


;---------------------------------------------------------------------------
; Returns the least common multiple (LCM) of self and the input argument.
; Uses the greatest common divisor (GCD) and the formula Knuth 4.5.2 (10):
;   u*v = GCD(u,v)*LCM(u,v)
;
function BigInteger::LCM, vNumIn
  compile_opt idl2, hidden
  on_error, 2
  vNum = ISA(vNumIn, /NUMBER) ? vNumIn : vNumIn.digits
  uNum = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  gcd = self.GCD(vNum)
  uv = BigInteger_Multiply(uNum, vNum)
  lcm = BigInteger_Divide(uv, gcd.digits)
  return, BigInteger(lcm)
end


;---------------------------------------------------------------------------
; Calculates a hashCode for this number.
;
function BigInteger::HashCode
  compile_opt idl2, hidden
  on_error, 2
  val = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  val = val.Hashcode()
  if (self.isNegative) then val = not val
  return, val
end


;---------------------------------------------------------------------------
function BigInteger::IsPowerOfTwo
  compile_opt idl2, hidden
  on_error, 2

  if (self.isNegative || ~PTR_VALID(self.pDigits)) then begin
    return, 0b
  endif

  digits = *self.pDigits
  powersOfTwo = 2uL^(LINDGEN(32))
  ; The high word must only have a single bit set.
  if (MAX(digits[-1] eq powersOfTwo) eq 0) then begin
    return, 0b
  endif
  ; If we only have a single "digit" then we are indeed a power of two.
  if (N_ELEMENTS(digits) eq 1) then begin
    return, 1b
  endif
  ; Otherwise, all lower words must be zero.
  success = ARRAY_EQUAL(digits[0:-2], 0)
  return, success
end


;---------------------------------------------------------------------------
; Generate a uniform random integer.
; By default the number is in the range 0...(2^bitLength - 1).
; If EXACT is set then the number is in the range
; 2^(bitLength - 1)...(2^bitLength - 1), in other words, the number is 
; guaranteed to have the specified number of bits.
;
function BigInteger::_GenerateRandomBits, bitLength, EXACT=exact, SEED=seed
  compile_opt idl2, hidden

  ran = RANDOMU(seed, (bitLength + 31LL)/32, /ULONG)
  ; If our random bit count isn't a multiple of 32, strip off excess bits.
  if (bitLength mod 32 ne 0) then begin
    ran[-1] = ran[-1] and (2uL^(bitLength mod 32) - 1)
  endif
;  nExtraBits = 32 - bitLength mod 32
;  if (nExtraBits ne 32) then begin
;    ran[-1] = ran[-1]/(2uL^nExtraBits)
;  endif

  if (KEYWORD_SET(exact)) then begin
    ; Make sure that the largest bit is set, so we actually get the
    ; desired number of bits.
    lastBit = (bitLength mod 32) - 1
    if (lastBit eq -1) then lastBit = 31
    ran[-1] = ran[-1] or (2uL^lastBit)
  endif else begin
    ; In the default case, we can get any number from 0 up to (2^bitLength - 1).
    ; Strip it off any leading zeroes.
    if (ran[-1] eq 0 && N_ELEMENTS(ran) ge 2) then begin
      ran = ran[0:-2]
    endif
  endelse
  return, ran
end


;---------------------------------------------------------------------------
; Generate a random integer > 1 and < N.
;
function BigInteger::_GenerateRandomRange, digits
  compile_opt idl2, hidden

  bitLength = self._bitLength(digits)
  while (1) do begin
    ran = self._GenerateRandomBits(bitLength)
    compare = BigInteger_Compare(ran, digits)
    ; Too big? Generate a new one.
    if (compare ge 0) then continue
    compare = BigInteger_Compare(ran, 1)
    ; Too small? Generate a new one.
    if (compare le 0) then continue
    ; Just right.
    break
  endwhile
  return, ran
end


;---------------------------------------------------------------------------
; Example: prob = 2d^(-100), k = 1024 bits
; Returns the number of iterations needed for the Miller-Rabin test,
; assuming that it is then followed by the Lucas test.
;
; Algorithm developed from the pseudocode in:
; NIST Federal Information Processing Standards 186-4,
; Digital Signature Standard (DSS), July 2013, section F1.1.
; Downloaded from http://csrc.nist.gov/publications/PubsFIPS.html, Dec 2013.
;
function BigInteger::_MillerRabinIterations, prob, k
  compile_opt idl2, hidden, static
  on_error, 2

  if (prob le 0 || prob ge 1 || k le 1) then return, 50

  tmax = -0.5*ALOG2(DOUBLE(prob))
  Mmax = FLOOR(2*SQRT(k - 1d) - 1)
  pconst = 2.00743d*ALOG(2)*k;*(2d^(-k))
  sumconst = (8*(!DPI^2 - 6)/3d)*(2d^(-2));(2d^(k-2))

  for t=2, tmax do begin
    for M=3, Mmax do begin
      mtotal = 0d
      for lm=3, M do begin
        j = [2:lm]
        jk = j + (k-1d)/j
        ; Subtract a large offset from our power of two, to avoid overflow.
        joffset = MAX(jk)/2
        jsum = TOTAL(1d/(2d^(jk - joffset)))
        mtotal += (2d^(lm - (lm-1)*t - joffset))*jsum
      endfor
      pkt = pconst*(2d^(-2d - M*t) + sumconst*mtotal)
      if (pkt le prob) then begin
        void = CHECK_MATH()
        return, t
      endif
    endfor
  endfor
  void = CHECK_MATH()
  return, -1
end


;---------------------------------------------------------------------------
; Knuth, section 4.5.4.
; Use simple factoring by division with the first several hundred primes.
; Return 1 if the number is possibly prime, 0 if definitely composite.
;
function BigInteger::_IsPrimeSmall, digits
  compile_opt idl2, hidden
  on_error, 2

  primes = self.GetPrimes(2000)
  if (N_ELEMENTS(digits) eq 1) then begin
    if (digits lt 2) then begin
      ; 0 and 1 are not prime numbers.
      return, 0b
    endif
    ; Maybe we actually have one of our prime numbers?
    if (MAX(digits[0] eq primes)) then begin
      ; Definitely a prime number.
      return, 1b
    endif
  endif

  ; See if the number is divisible by one of the primes from our table.
  foreach p, primes do begin
    !null = BigInteger_Divide(digits, p, remainder)
    if (ARRAY_EQUAL(remainder, 0)) then begin
      return, 0b
    endif
  endforeach
  ; Possibly a prime number (not divisible by any of our small primes).
  return, 1b
end


;---------------------------------------------------------------------------
; Knuth, section 4.5.4, algorithm P
; This is the Miller-Rabin probabilistic test.
; Returns 0 (false) if the number is definitely not prime.
; Returns 1 (true) if the number is probably prime,
; with the probability of being wrong given as (1/4)^Niter,
; where Niter is the number of iterations.
; So the default of 25 iterations will be wrong with a probability less
; than (1/4)^25 ~ 1x10^(-15).
;
function BigInteger::_IsPrimeMillerRabin, digits, ITERATIONS=iterationsIn
  compile_opt idl2, hidden
  on_error, 2
  
  if (N_ELEMENTS(digits) eq 1) then begin
    if (digits lt 2) then return, 0b
    if (digits eq 2) then return, 1b
  endif
  ; Is our number divisible by 2?
  if (digits[0] mod 2 eq 0) then begin
    return, 0b
  endif

  if (ISA(iterationsIn)) then begin
    iterations = iterationsIn
  endif else begin
    bits = self._BitLength(digits)
    ; This number of iterations will give a probability of 2^(-100)
    ; of falsely identifying a number as prime when it is not.
    case (1) of
      (bits ge 3072): iterations = 2
      (bits ge 2048): iterations = 2
      (bits ge 1024): iterations = 4
      (bits ge 512): iterations = 7
      (bits ge 256): iterations = 16
      (bits ge 128): iterations = 34
      (bits ge 64): iterations = 44
      (bits ge 32): iterations = 49
      else: iterations = 64
    endcase
  endelse

  ; For n = 1 + (2^k)q, determine k and q
  nMinus1 = BigInteger_Subtract(digits, 1)
  k = 0uL
  q = nMinus1
  while (1) do begin
    qtmp = BigInteger_Divide(q, 2, remainder)
    if (remainder ne 0) then break
    k++
    q = qtmp
  endwhile
  
  for iter=1,iterations do begin

    ; step P1, generate random x
    xx = self._GenerateRandomRange(digits)

    ; step P2, exponentiate, y = x^q mod n
    yy = self._ModPower(xx, q, digits)

    j = 0
  
    while (1) do begin
  
      ; step P3, is n "probably prime"?
      if (BigInteger_Compare(yy, 1) eq 0) then begin
        ; if j is still 0, then probably prime, otherwise definitely not prime
        if (j gt 0) then begin
          return, 0b
        endif
        ; Break out of the while, and try another random X.
        break
      endif
  
      if (BigInteger_Compare(yy, nMinus1) eq 0) then begin
        ; probably prime
        ; Break out of the while, and try another random X.
        break
      endif
      
      ; step P4, increase j
      j++
      if (j eq k) then begin
        ; failure, definitely not prime
        return, 0b
      endif
      ; set y to y^2 mod n
      tmp = BigInteger_Multiply(yy, yy)
      !null = BigInteger_Divide(tmp, digits, remainder)
      yy = remainder
    endwhile
  endfor

  ; We conclude that N is probably prime
  ; with probability 0.25^iterations that we are wrong.
  return, 1b
end


;---------------------------------------------------------------------------
; Calculates the Jacobi symbol (a/N), used for the Lucas primality test.
;
; Algorithm developed from the pseudocode in:
; NIST Federal Information Processing Standards 186-4,
; Digital Signature Standard (DSS), July 2013, section C3.3.
; Downloaded from http://csrc.nist.gov/publications/PubsFIPS.html, Dec 2013.
;
function BigInteger::_JacobiSymbol, aIntIn, digits
  compile_opt idl2, hidden
  on_error, 2

  aInt = (aIntIn ge 0) ? aIntIn : BigInteger_Subtract(digits, ABS(aIntIn))
  !null = BigInteger_Divide(aInt, digits, remainder)
  aInt = TEMPORARY(remainder)
  if (BigInteger_Compare(aInt, 1) eq 0 || $
    BigInteger_Compare(digits, 1) eq 0) then begin
    return, 1
  endif
  if (BigInteger_Compare(aInt, 0) eq 0) then begin
    return, 0
  endif

  ; Rewrite aInt as aInt = (2^e)*a1, determine e and a1
  e = 0
  a1 = aInt
  while (1) do begin
    tmp = BigInteger_Divide(a1, 2, remainder)
    if (BigInteger_Compare(remainder, 0) ne 0) then break
    a1 = tmp
    e++
  endwhile
  
  if (e[0] mod 2 eq 0) then begin
    s = 1
  endif else begin
    !null = BigInteger_Divide(digits, 8, remainder)
    case (remainder) of
      1: s = 1
      7: s = 1
      3: s = -1
      5: s = -1
    endcase
  endelse
  !null = BigInteger_Divide(digits, 4, nMod4)
  !null = BigInteger_Divide(a1, 4, a1Mod4)
  if (nMod4 eq 3 && a1Mod4 eq 3) then begin
    s = -s
  endif

  !null = BigInteger_Divide(digits, a1, n1)
  
  ; Call recursively.
  result = s*self._JacobiSymbol(n1, a1)
  return, result
end


;---------------------------------------------------------------------------
; Lucas primality test. Returns 1 (true) if the number given in digits
; is probably prime, and 0 (false) if the number is definitely composite.
;
; Algorithm developed from the pseudocode in:
; NIST Federal Information Processing Standards 186-4,
; Digital Signature Standard (DSS), July 2013, section C3.3.
; Downloaded from http://csrc.nist.gov/publications/PubsFIPS.html, Dec 2013.
;
function BigInteger::_IsPrimeLucas, digits
  compile_opt idl2, hidden
  on_error, 2

  ; Check for 0, 1, 2, 3.
  if (N_ELEMENTS(digits) eq 1) then begin
    if (digits le 1) then begin
      return, 0b
    endif
    ; For small primes just use our division algorithm.
    ; Some of these will fail the Lucas test because of the JacobiSymbol.
    if (digits le 3996001) then begin
      return, self._IsPrimeSmall(digits)
    endif
  endif
  ; Is our number divisible by 2?
  if (digits[0] mod 2 eq 0) then begin
    return, 0b
  endif

  ; Check for perfect squares
  sq = self._Sqrt(digits)
  squared = BigInteger_Multiply(sq, sq)
  if (BigInteger_Compare(squared, digits) eq 0) then begin
    return, 0b
  endif
  
  ; Find the first D in the sequence 5, -7, 9, -11, 13, -15, 17,...
  ; where the Jacobi symbol = -1.
  dd = 5
  while (1) do begin
    js = self._JacobiSymbol(dd, digits)
    if (js eq 0) then begin
      return, 0b  ; composite
    endif
    ; Did we find our D?
    if (js eq -1) then break
    ; 5, -7, 9, -11, 13, -15, 17,...
    dd = -dd + ((dd gt 0) ? -2 : 2)
    if (ABS(dd) gt 100000) then stop
  endwhile
  
  isNeg = (dd lt 0)
  dd = ABS(dd)

  ; Find the binary expansion of N + 1.
  kk = BigInteger_Add(digits, 1)
  kbits = self._Convert(kk, 2)
  Ui = 1
  Vi = 1

  for i=N_ELEMENTS(kbits)-2, 0, -1 do begin

    ; step 6.1, Utemp = (U_{i+1})(V_{i+1}) mod N
    UV = BigInteger_Multiply(Ui, Vi)
    !null = BigInteger_Divide(UV, digits, Utemp)

    ; step 6.2, Vtemp = [(V_{i+1})^2 + D*(U_{i+1})^2]/2 mod N
    V2 = BigInteger_Multiply(Vi, Vi)
    DU2 = BigInteger_Multiply(dd, BigInteger_Multiply(Ui, Ui))
    isNegV2plusDU2 = 0b
    if (isNeg) then begin
      if (BigInteger_Compare(V2, DU2) ge 0) then begin
        V2plusDU2 = BigInteger_Subtract(V2, DU2)
      endif else begin
        V2plusDU2 = BigInteger_Subtract(DU2, V2)
        isNegV2plusDU2 = 1b
      endelse
    endif else begin
      V2plusDU2 = BigInteger_Add(V2, DU2)
    endelse

    ; If V^2 + DU^2 is odd, then add N to it before dividing by 2.
    if (V2plusDU2[0] mod 2) then begin
      if (isNegV2plusDU2) then begin
        ; If V^2 + DU^2 is negative and bigger than N, we will still have a negative.
        if (BigInteger_Compare(V2plusDU2, digits) ge 0) then begin
          V2plusDU2 = BigInteger_Subtract(V2plusDU2, digits)
        endif else begin
          ; Otherwise, we will now be positive.
          V2plusDU2 = BigInteger_Subtract(digits, V2plusDU2)
          isNegV2plusDU2 = 0b
        endelse
      endif else begin
        V2plusDU2 = BigInteger_Add(V2plusDU2, digits)
      endelse
    endif

    V2plusDU2 = BigInteger_Divide(V2plusDU2, 2)

    !null = BigInteger_Divide(V2plusDU2, digits, Vtemp)

    ; If (V^2 + DU^2)/2 was negative, then add N to make it a positive mod.
    if (isNegV2plusDU2) then begin
      Vtemp = BigInteger_Subtract(digits, Vtemp)
    endif

    ; step 6.3
    if (kbits[i] eq 1) then begin
      ; steps 6.3.1-6.3.2
      UplusV = BigInteger_Add(Utemp, Vtemp)
      ; If Utemp + Vtemp is odd, then add N to it before dividing by 2.
      if (UplusV[0] mod 2) then begin
        UplusV = BigInteger_Add(UplusV, digits)
      endif
      UplusV2 = BigInteger_Divide(UplusV, 2)
      !null = BigInteger_Divide(UplusV2, digits, Ui)

      DU = BigInteger_Multiply(dd, Utemp)

      isNegVplusDU = 0b
      if (isNeg) then begin
        if (BigInteger_Compare(Vtemp, DU) ge 0) then begin
          VplusDU = BigInteger_Subtract(Vtemp, DU)
        endif else begin
          VplusDU = BigInteger_Subtract(DU, Vtemp)
          isNegVplusDU = 1b
        endelse
      endif else begin
        VplusDU = BigInteger_Add(Vtemp, DU)
      endelse

      ; If Vtemp + DUtemp is odd, then add N to it before dividing by 2.
      if (VplusDU[0] mod 2) then begin
        if (isNegVplusDU) then begin
          ; If V + DU is negative and bigger than N, we will still have a negative.
          if (BigInteger_Compare(VplusDU, digits) ge 0) then begin
            VplusDU = BigInteger_Subtract(VplusDU, digits)
          endif else begin
            ; Otherwise, we will now be positive.
            VplusDU = BigInteger_Subtract(digits, VplusDU)
            isNegVplusDU = 0b
          endelse
        endif else begin
          VplusDU = BigInteger_Add(VplusDU, digits)
        endelse
      endif

      VplusDU2 = BigInteger_Divide(VplusDU, 2)

      !null = BigInteger_Divide(VplusDU2, digits, Vi)

      ; If (Vtmp + DUtmp)/2 was negative, then add N to make it a positive mod.
      if (isNegVplusDU) then begin
        Vi = BigInteger_Subtract(digits, Vi)
      endif

    endif else begin
      ; steps 6.3.3, 6.3.4
      Ui = TEMPORARY(Utemp)
      Vi = TEMPORARY(Vtemp)
    endelse
  endfor
  
  ; If U0 == 0, then probably prime, otherwise composite
  result = (BigInteger_Compare(Ui, 0) eq 0) ? 1b : 0b
  return, result
end


;---------------------------------------------------------------------------
; Internal routine.
; If the number passes all tests, then assume it is prime.
;
function BigInteger::_IsPrime, digits, ITERATIONS=iterations
  compile_opt idl2, hidden
  on_error, 2

  if (~self._IsPrimeSmall(digits)) then begin
    return, 0b
  endif

  ; If our number is smaller than our largest prime^2, we've succeeded.
  if (N_ELEMENTS(digits) eq 1 && digits le 3996001) then begin
    return, 1b
  endif

  if (~self._IsPrimeMillerRabin(digits, ITERATIONS=iterations)) then begin
    return, 0b
  endif

  if (~self._IsPrimeLucas(digits)) then begin
    return, 0b
  endif
  return, 1b
end


;---------------------------------------------------------------------------
; Returns 1 (true) if this number is probably prime, 0 (false) if this
; number is definitely composite.
; Uses three tests:
;   (1) division by all small primes up to 997
;   (2) multiple iterations of the Miller-Rabin test
;   (3) a final test using Lucas
;
; For the Miller-Rabin test, the number of iterations is taken from
; Table C.1 (Minimum number of Miller-Rabin iterations for DSA) in:
; NIST Federal Information Processing Standards 186-4,
; Digital Signature Standard (DSS), July 2013, section C3.
; Downloaded from http://csrc.nist.gov/publications/PubsFIPS.html, Dec 2013.
;
function BigInteger::IsPrime, ITERATIONS=iterations
  compile_opt idl2, hidden
  on_error, 2
  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0
  return, self._IsPrime(digits, ITERATIONS=iterations)
end


;---------------------------------------------------------------------------
function BigInteger::Log2, REMAINDER=remainder
  compile_opt idl2, hidden
  on_error, 2

  bits = self.BitLength() - 1

  if (self.isNegative || bits lt 0) then begin
    MESSAGE, 'Unable to compute Log2() of a non-positive BigInteger.'
  endif

  if (ARG_PRESENT(remainder)) then begin
    this = self
    remainder = this - BigInteger(2)^bits
  endif
  return, bits
end


;---------------------------------------------------------------------------
function BigInteger::Log10, REMAINDER=remainder
  compile_opt idl2, hidden
  on_error, 2

  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  ndigits = N_ELEMENTS(digits)

  if (self.isNegative || ARRAY_EQUAL(digits, 0uL)) then begin
    MESSAGE, 'Unable to compute Log10() of a non-positive BigInteger.'
  endif

  ; We only need a few digits to reach the limit of double precision.
  result = 0d
  nstop = (ndigits - 5) > 0
  for i=ndigits-1,nstop,-1 do begin
    result += (2d)^((i-nstop)*32)*digits[i]
  endfor

  log10 = LONG64((32d*nstop)*alog10(2d) + alog10(result))

  if (ARG_PRESENT(remainder)) then begin
    this = self
    remainder = this - BigInteger(10)^log10
  endif

;  log2 = self.Log2()
;  log10 = ULONG64((log2 + 1)/ALOG2(10d))
  
  ; About 50% of the time the log10 will be 1 too large.
;  power10 = self._Exponent(10, log10)
;  if (BigInteger_Compare(power10, digits) eq 1) then begin
;    log10--
;    if (ARG_PRESENT(remainder)) then begin
;      power10 = BigInteger_Divide(power10, 10)
;    endif
;  endif

;  if (ARG_PRESENT(remainder)) then begin
;    remainder = BigInteger_Subtract(digits, power10)
;    remainder = BigInteger(remainder)
;  endif

  return, log10
end


;---------------------------------------------------------------------------
; compute the multiplicative inverse of self modulo the modulusIn.
; In order for the inverse to exist, "self" and "modulusIn" must be
; co-prime (i.e. their GCD must be 1).
; If the modInverse exists, then: self*result mod modulusIn == 1.
; See the Wikipedia page on the Extended Euclidean Algorithm.
;
function BigInteger::ModInverse, modulusIn
  compile_opt idl2, hidden
  on_error, 2

  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  modulus = ISA(modulusIn, 'BigInteger') ? modulusIn : BigInteger(modulusIn)

  modulus = modulus.digits
  t = 0uL
  tNew = 1uL
  r = modulus
  rNew = digits
  tNegative = 1b
  
  while (BigInteger_Compare(rNew, 0) ne 0) do begin
    quotient = BigInteger_Divide(r, rNew)
    ; Each time through the loop the sign of t flips
    tNegative = ~tNegative
    tPrev = t
    t = tNew
    tTmp =  BigInteger_Multiply(quotient, tNew)
    tNew = BigInteger_Add(tPrev, tTmp)
    rPrev = r
    r = rNew
    rTmp =  BigInteger_Multiply(quotient, rNew)
    rNew = BigInteger_Subtract(rPrev, rTmp)
  endwhile
  if (r gt 1) then begin
    MESSAGE, 'Value is not invertible'
  endif
  if (tNegative) then t = BigInteger_Subtract(modulus, t)
  return, BigInteger(t)

  ;*** Unused
  ; Original algorithm using operator overloading.
  ; 20 times slower but more readable...
  t = BigInteger(0uL)
  tNew = BigInteger(1uL)
  r = modulus
  rNew = self
  while (rNew ne 0) do begin
    quotient = r/rNew
    tPrev = t
    t = tNew
    tNew = tPrev - quotient*tNew
    rPrev = r
    r = rNew
    rNew = rPrev - quotient*rNew
;    print, t.toInteger(), tNew.toInteger(), r.toInteger(), rNew.toInteger()
  endwhile
  if (r gt 1) then begin
    MESSAGE, 'Value is not invertible'
  endif
  if (t lt 0) then t = t + modulus
  return, t
end


;---------------------------------------------------------------------------
function BigInteger::_ModPower, base, exponent, modulus
  compile_opt idl2, hidden

  result = 1uL
  while (BigInteger_Compare(exponent, 0) eq 1) do begin
    exponent = BigInteger_Divide(exponent, 2, remainder)
    if (remainder eq 1) then begin
      tmp = BigInteger_Multiply(result, base)
      !null = BigInteger_Divide(tmp, modulus, remainder)
      result = remainder
    endif
    tmp = BigInteger_Multiply(base, base)
    !null = BigInteger_Divide(tmp, modulus, remainder)
    base = remainder
  endwhile

  return, result
end


;---------------------------------------------------------------------------
; compute  self^exponent mod modulus
; Uses the right-to-left binary method, as described on the Wikipedia
; page for Modular Exponentiation. The exponent is broken down into binary bits.
; The result is then the product of each power of two (if the bit is set)
; modulus the modulusIn.
; Note that Montgomery reduction could be used instead, but the code
; becomes extremely complicated.
;
function BigInteger::ModPower, exponentIn, modulusIn
  compile_opt idl2, hidden
  on_error, 2

  base = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  exponent = ISA(exponentIn, 'BigInteger') ? exponentIn : BigInteger(exponentIn)
  exponent = exponent.digits
  modulus = ISA(modulusIn, 'BigInteger') ? modulusIn : BigInteger(modulusIn)
  modulus = modulus.digits
  result = self._ModPower(base, exponent, modulus)
  return, BigInteger(result)
end


;---------------------------------------------------------------------------
; Generate the next prime number following this one.
;
function BigInteger::_NextPrime, digits
  compile_opt idl2, hidden
  on_error, 2
  
  primes = self.GetPrimes()
  ; For small primes just return the next one in our built-in list.
  if (N_ELEMENTS(digits) eq 1 && digits lt primes[-1]) then begin
    next = primes[(WHERE(primes gt digits))[0]]
    return, next
  endif
  
  ; If self is even, subtract 1.
  if ((digits[0] mod 2) eq 0) then begin
    digits = BigInteger_Subtract(digits, 1)
  endif

  counter = 0uL
  iter = 0uL

  while (1) do begin
    skip = 0
    while (1) do begin
      skip++
      counter++
      if (counter ge N_ELEMENTS(bitSieve)) then begin
        skip = 1
        counter = 1uL
        bitSieve = BYTARR(10000) + 1b
        foreach prime, primes do begin
          !null = BigInteger_Divide(digits, prime, remainder)
          diff = prime - remainder
          bitSieve[diff:*:prime] = 0b
        endforeach
;        print, total(bitsieve)/n_elements(bitsieve), min(where(bitsieve[1:*])) + 1
      endif
      if (bitSieve[counter]) then break
    endwhile
    digits = BigInteger_Add(digits, skip)
    iter++
    if (self._IsPrime(digits)) then begin
      break
    endif
  endwhile

  return, digits
end


;---------------------------------------------------------------------------
; Generate the next prime number following this one.
;
function BigInteger::NextPrime
  compile_opt idl2, hidden
  on_error, 2
  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0
  nextprime = self._NextPrime(digits)
  return, BigInteger(nextprime)
end


;---------------------------------------------------------------------------
; Generate a random prime number with a specified number of bits.
;
function BigInteger::Prime, bitLength, SEED=seed
  compile_opt idl2, hidden, static
  on_error, 2
  if (N_ELEMENTS(bitLength) ne 1 || bitLength lt 2) then begin
    MESSAGE, 'Bit length must be an integer >= 2.'
  endif
  this = BigInteger(2)
  while (1) do begin
    if (ISA(seed)) then begin
      digits = this._GenerateRandomBits(bitLength, /EXACT, SEED=seed)
    endif else begin
      ;*** If user did not pass in seed, do not pass in a seed variable.
      ; Otherwise, this method will keep generating the same random primes
      ; because IDL's internal seed will only get incremented once.
      digits = this._GenerateRandomBits(bitLength, /EXACT)
    endelse
    if (this._IsPrime(digits)) then begin
      break
    endif
  endwhile

  return, BigInteger(digits)
end


;---------------------------------------------------------------------------
; Helper function for Primorial, used to recursively multiply arrays of digits.
;
function BigInteger::_PrimorialMultiply, left, right
  compile_opt idl2, hidden
  on_error, 2

  nleft = N_ELEMENTS(left)
  nright = N_ELEMENTS(right)
  if (nleft gt 1) then begin
    left = self._PrimorialMultiply(left[0:nleft/2-1], left[nleft/2:*])
  endif
  if (nright gt 1) then begin
    right = self._PrimorialMultiply(right[0:nright/2-1], right[nright/2:*])
  endif
  result = BigInteger_Multiply(left, right)
  return, result
end


;---------------------------------------------------------------------------
; Generate the prime factorial of N.
;
; The primorial is defined as the product of all prime numbers
; less than or equal to N (N may be a prime number or a composite number).
;
; If IS_INDEX is set then N is interpreted as an index into the sequence
; of prime numbers. In this case the primorial is the product of all
; prime numbers up to and including the N-th prime.
;
; COUNT is an output keyword that is set equal to the number of prime
; numbers that were included in the product. If IS_INDEX is set then
; the COUNT will be equal to N.
;
function BigInteger::Primorial, n, COUNT=count, IS_INDEX=isIndexIn
  compile_opt idl2, hidden, static
  on_error, 2

  nextprime = 2uL
  digits = 2uL
  this = BigInteger()
  isIndex = KEYWORD_SET(isIndexIn)
  primes = this.GetPrimes(n, IS_INDEX=isIndex, COUNT=count)
  if (N_ELEMENTS(primes) gt 1) then begin
    result = this._PrimorialMultiply(primes[0:count/2-1],primes[count/2:*])
  endif else begin
    result = primes
  endelse
  return, BigInteger(result)
end


;---------------------------------------------------------------------------
; Generate a random number with a specified number of bits.
;
function BigInteger::Random, bitLength, EXACT=exact, SEED=seed
  compile_opt idl2, hidden, static
  on_error, 2
  if (N_ELEMENTS(bitLength) ne 1 || bitLength lt 1) then begin
    MESSAGE, 'Bit length must be an integer >= 1.'
  endif
  this = BigInteger()
  digits = this._GenerateRandomBits(bitLength, EXACT=exact, SEED=seed)
  return, BigInteger(digits)
end


;---------------------------------------------------------------------------
function BigInteger::Signum
  compile_opt idl2, hidden
  on_error, 2
  compare = self._compareTo(self, 0)
  return, compare
end


;---------------------------------------------------------------------------
; Internal routine to compute the square root of N (given by digits),
; using Newton's method.
;
function BigInteger::_Sqrt, digits
  compile_opt idl2, hidden
  on_error, 2

  ; Compute the sqrt of the largest digit,
  ; and then shift left by half of the remaining bits.
  nbits = 16L*(N_ELEMENTS(digits) - 1)
  powerOfTwo = self._Exponent(2, nbits)
  guess = BigInteger_Multiply(ROUND(SQRT(digits[-1])), powerOfTwo)

  while (1) do begin
    ; newguess = (guess + myself/guess)/2
    newguess = BigInteger_Divide(BigInteger_Add(guess, BigInteger_Divide(digits, guess)), 2)
    ; If the new guess is equal to the old guess, then we've converged.
    if (BigInteger_Compare(newguess, guess) eq 0) then break
    guess = newguess
  endwhile

  return, newguess
end


;---------------------------------------------------------------------------
function BigInteger::Sqrt, REMAINDER=remainder
  compile_opt idl2, hidden
  on_error, 2

  if (~PTR_VALID(self.pDigits)) then begin
    return, BigInteger(0)
  endif
  if (self.isNegative) then begin
    MESSAGE, 'Unable to compute SQRT() of a negative BigInteger.'
  endif

  myself = self
  bitLength = self.bitLength()

  ; If we can fit into a long integer, just compute the sqrt directly.
  if (bitLength le 62 ) then begin
    result = BigInteger(Sqrt(self.toDouble()))
  endif else begin
    result = self._Sqrt(*self.pDigits)
    result = BigInteger(result)
  endelse

  if (ARG_PRESENT(remainder)) then begin
    remainder = myself - result*result
  endif
  return, result
end


;---------------------------------------------------------------------------
function BigInteger::ToDouble, EXPONENT=exponent
  compile_opt idl2, hidden
  on_error, 2

  if (~PTR_VALID(self.pDigits)) then begin
    return, 0d
  endif

  result = 0d
  ndigits = N_ELEMENTS(*self.pDigits)
  ; We only need a few digits to reach the limit of double precision.
  nstop = (ndigits - 5) > 0
  for i=ndigits-1,nstop,-1 do begin
    result += (2d)^((i-nstop)*32)*((*self.pDigits)[i])
  endfor

  if (ARG_PRESENT(exponent)) then begin
    log10result = (32d*nstop)*alog10(2d) + alog10(result)
    exponent = LONG64(log10result)
    div = BigInteger(10)^((exponent - 20) > 0)
    this = self
    q = this/div
    result = q.toString()
    result = DOUBLE(STRMID(result,0,1)+'.'+STRMID(result,1))
  endif else begin
    result = (2d^(32*nstop))*result
  endelse

  if (self.isNegative) then begin
    result = -result
  endif
  ; Suppress overflow for Infinity
  void = CHECK_MATH()

  return, result
end


;---------------------------------------------------------------------------
; Returns a signed 64-bit integer.
;
function BigInteger::ToInteger
  compile_opt idl2, hidden
  on_error, 2

  if PTR_VALID(self.pDigits) then begin
    ndigits = N_ELEMENTS(*self.pDigits)
    if (ndigits gt 2) then begin
      MESSAGE, /INFO, 'Integer overflow.'
      result = 0LL
    endif else if (ndigits eq 2) then begin
      result = LONG64((*self.pDigits)[0] + (2uLL^32)*((*self.pDigits)[1]))
      if (result lt 0) then begin
        MESSAGE, /INFO, 'Integer overflow.'
        result = 0LL
      endif
    endif else begin
      result = LONG64((*self.pDigits)[0])
    endelse
    if (self.isNegative) then begin
      result = -result
    endif
  endif else begin
    result = 0LL
  endelse

  return, result
end


;---------------------------------------------------------------------------
function BigInteger::ToString, RADIX=radixIn, UPPERCASE=toUpper
  compile_opt idl2, hidden
  on_error, 2

  radix = ISA(radixIn) ? radixIn[0] : 10
  if (radix lt 2 || radix gt 36) then begin
    MESSAGE, 'Radix must be between 2 and 36.'
  endif
  
  upper = KEYWORD_SET(toUpper)
  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL

  ; Shortcut for special radixes
  if (radix eq 10) then begin
    newDigits = self._Convert(digits, 1000000000L)
    newDigits = REVERSE(newDigits, /OVERWRITE) 
    result = STRING(newDigits[0], FORMAT='(I0)')
    if (N_ELEMENTS(newDigits) gt 1) then begin
      result += STRJOIN(STRING(newDigits[1:*], FORMAT='(I9.9)'))
    endif
  endif else if (radix eq 16) then begin
    newDigits = REVERSE(digits)
    result = STRING(newDigits[0], FORMAT=upper ? '(Z0)' : '(z0)')
    if (N_ELEMENTS(newDigits) gt 1) then begin
      result += STRJOIN(STRING(newDigits[1:*], $
        FORMAT=upper ? '(Z8.8)' : '(z8.8)'))
    endif
  endif else if (radix eq 2) then begin
    newDigits = REVERSE(digits)
    result = STRING(newDigits[0], FORMAT='(b0)')
    if (N_ELEMENTS(newDigits) gt 1) then begin
      result += STRJOIN(STRING(newDigits[1:*], FORMAT='(b32.32)'))
    endif
  endif else if (radix eq 8) then begin
    newDigits = self._Convert(digits, 8uL^10)
    newDigits = REVERSE(newDigits, /OVERWRITE)
    result = STRING(newDigits[0], FORMAT='(o0)')
    if (N_ELEMENTS(newDigits) gt 1) then begin
      result += STRJOIN(STRING(newDigits[1:*], FORMAT='(o10.10)'))
    endif
  endif else begin
    ; For other radixes, find the digits in the new radix, run them through
    ; the digit . character conversion, and then join all the characters.
    newDigits = BigInteger_Convert(digits, radix)
    newDigits = REVERSE(newDigits, /OVERWRITE)
    ; [0-9, A-Z] or [0-9, a-z]
    bytes = upper ? [([48b:57b]), ([65b:90b])] : [([48b:57b]), ([97b:122b])]
    result = STRJOIN(bytes[newDigits])
  endelse

  if (self.isNegative && ~ARRAY_EQUAL(digits, 0)) then begin
    result = '-' + result
  endif

  return, result
end


;---------------------------------------------------------------------------
pro BigInteger::_bitWiseOperatorArgs, arg1, arg2, smaller, larger
  compile_opt idl2, hidden
  on_error, 2
  var1 = ISA(arg1, 'BigInteger') ? arg1 : BigInteger(arg1)
  var2 = ISA(arg2, 'BigInteger') ? arg2 : BigInteger(arg2)
  var1.GetProperty, DIGITS=digits1
  var2.GetProperty, DIGITS=digits2
  oneIsSmaller = N_ELEMENTS(digits1) le N_ELEMENTS(digits2)
  smaller = oneIsSmaller ? digits1 : digits2
  larger = oneIsSmaller ? digits2 : digits1
end


;---------------------------------------------------------------------------
function BigInteger::_overloadAND, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2

  self._bitWiseOperatorArgs, arg1, arg2, smaller, larger
  ; For the "AND" operator, any extra digits in the larger integer will be tossed.
  result = smaller
  for i=0,N_ELEMENTS(smaller)-1 do begin
    result[i] = smaller[i] and larger[i]
  endfor
  return, BigInteger(result)
end


;---------------------------------------------------------------------------
function BigInteger::_overloadAsterisk, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2

  var1 = ISA(arg1, 'BigInteger') ? arg1 : BigInteger(arg1)
  var2 = ISA(arg2, 'BigInteger') ? arg2 : BigInteger(arg2)
  var1.GetProperty, DIGITS=digits1, SIGN=sign1
  var2.GetProperty, DIGITS=digits2, SIGN=sign2

  result = BigInteger_Multiply(digits1, digits2)

  sign = (sign1 ne sign2) ? -1 : 1
  return, BigInteger(result, SIGN=sign)
end


;---------------------------------------------------------------------------
function BigInteger::_overloadCaret, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2

  var1 = ISA(arg1, 'BigInteger') ? arg1 : BigInteger(arg1)
  var1.GetProperty, DIGITS=digits1, SIGN=sign1
  exponent = ISA(arg2, 'BigInteger') ? arg2.toInteger() : LONG64(arg2)
  if (exponent lt 0) then begin
    MESSAGE, 'Exponent must be a non-negative integer.'
  endif

  result = self._Exponent(digits1, exponent)

  ; A negative number will only stay negative for an odd exponent.
  sign = (sign1 eq -1 && (exponent mod 2) eq 1) ? -1 : 1
  return, BigInteger(result, SIGN=sign)
end


;---------------------------------------------------------------------------
function BigInteger::_overloadEQ, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._compareTo(arg1, arg2)
  return, result eq 0
end


;---------------------------------------------------------------------------
function BigInteger::_overloadNE, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._compareTo(arg1, arg2)
  return, result ne 0
end


;---------------------------------------------------------------------------
function BigInteger::_overloadGT, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._compareTo(arg1, arg2)
  return, result eq 1
end


;---------------------------------------------------------------------------
function BigInteger::_overloadGE, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._compareTo(arg1, arg2)
  return, result ge 0
end


;---------------------------------------------------------------------------
function BigInteger::_overloadLT, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._compareTo(arg1, arg2)
  return, result eq -1
end


;---------------------------------------------------------------------------
function BigInteger::_overloadLE, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._compareTo(arg1, arg2)
  return, result le 0
end


;---------------------------------------------------------------------------
function BigInteger::_overloadGreaterThan, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  compare = self._compareTo(arg1, arg2)
  winner = (compare ge 0) ? arg1 : arg2
  return, ISA(winner, 'BigInteger') ? winner : BigInteger(winner)
end


;---------------------------------------------------------------------------
function BigInteger::_overloadHelp, varname
  compile_opt idl2, hidden
  on_error, 2

  n = self.BitLength()
   if (n le 64) then begin
    value = self.toString()
  endif else begin
    value = self.toDouble(EXPONENT=exponent)
    value = STRTRIM(STRING(value, /IMPLIED),2) + $
      '...x10^' + STRTRIM(exponent,2)
  endelse
  result = varname
  slen = STRLEN(varname)
  if (slen lt 15) then result += STRJOIN(REPLICATE(' ', 15 - slen))
  result += ' ' + OBJ_CLASS(self) + $
    ' <ID=' + STRTRIM(OBJ_VALID(self,/GET_HEAP_ID),2) + $
    ' LENGTH=' + STRTRIM(n,2) + ' bits> = ' + value

  return, result

end


;---------------------------------------------------------------------------
function BigInteger::_overloadImpliedPrint, varname
  compile_opt idl2, hidden
  on_error, 2
  return, self.toString()
end


;---------------------------------------------------------------------------
; Return 1 (true) if our value is nonzero, otherwise return 0 (false).
;
function BigInteger::_overloadIsTrue
  compile_opt idl2, hidden
  on_error, 2
  compare = self._compareTo(self, 0)
  return, compare ne 0
end


;---------------------------------------------------------------------------
function BigInteger::_overloadLessThan, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  compare = self._compareTo(arg1, arg2)
  winner = (compare eq 1) ? arg2 : arg1
  return, ISA(winner, 'BigInteger') ? winner : BigInteger(winner)
end


;---------------------------------------------------------------------------
function BigInteger::_overloadMinus, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._addDigitsWithSigns(arg1, arg2, /SUBTRACT)
  return, result
end


;---------------------------------------------------------------------------
function BigInteger::_overloadMinusUnary
  compile_opt idl2, hidden
  on_error, 2

  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0
  sign = (self.isNegative) ? 1 : -1
  result = BigInteger(digits, SIGN=sign)
  return, result
end


;---------------------------------------------------------------------------
function BigInteger::_overloadMod, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  var1 = ISA(arg1, 'BigInteger') ? arg1 : BigInteger(arg1)
  var2 = ISA(arg2, 'BigInteger') ? arg2 : BigInteger(arg2)
  result = var1._Divide(var2, REMAINDER=remainder)
  return, remainder
end


;---------------------------------------------------------------------------
function BigInteger::_overloadNOT
  compile_opt idl2, hidden
  on_error, 2

  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0uL
  for i=0,N_ELEMENTS(digits)-1 do begin
    digits[i] = not digits[i]
  endfor
  return, BigInteger(digits)
end


;---------------------------------------------------------------------------
function BigInteger::_overloadOR, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2

  self._bitWiseOperatorArgs, arg1, arg2, smaller, larger
  ; For the "OR" operator, any extra digits in the larger integer will be kept.
  result = larger
  for i=0,N_ELEMENTS(smaller)-1 do begin
    result[i] = smaller[i] or larger[i]
  endfor
  return, BigInteger(result)
end


;---------------------------------------------------------------------------
function BigInteger::_overloadPlus, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  result = self._addDigitsWithSigns(arg1, arg2)
  return, result
end


;---------------------------------------------------------------------------
pro BigInteger::_overloadPlusPlus
  compile_opt idl2, hidden
  on_error, 2
  result = self._addDigitsWithSigns(self, 1)
  result.GetProperty, DIGITS=digits, SIGN=sign
  *self.pDigits = digits
  self.isNegative = sign eq -1
end


;---------------------------------------------------------------------------
pro BigInteger::_overloadMinusMinus
  compile_opt idl2, hidden
  on_error, 2
  result = self._addDigitsWithSigns(self, -1)
  result.GetProperty, DIGITS=digits, SIGN=sign
  *self.pDigits = digits
  self.isNegative = sign eq -1
end


;---------------------------------------------------------------------------
; If user does a "post-increment" or "post-decrement", such as B=A++,
; we need to create a new BigInteger to return for B, so that it doesn't
; change when A gets incremented.
;
function BigInteger::_overloadPostIncrementCopy
  compile_opt idl2, hidden
  on_error, 2
  digits = PTR_VALID(self.pDigits) ? *self.pDigits : 0
  sign = (self.isNegative) ? -1 : 1
  result = BigInteger(digits, SIGN=sign)
  return, result
end


;---------------------------------------------------------------------------
function BigInteger::_overloadPrint
  compile_opt idl2, hidden
  on_error, 2
  return, self.toString()
end


;---------------------------------------------------------------------------
function BigInteger::_overloadSize
  compile_opt idl2, hidden
  on_error, 2
  return, 1
end


;---------------------------------------------------------------------------
function BigInteger::_overloadSlash, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2
  var1 = ISA(arg1, 'BigInteger') ? arg1 : BigInteger(arg1)
  var2 = ISA(arg2, 'BigInteger') ? arg2 : BigInteger(arg2)
  result = var1._Divide(var2)
  return, result
end


;---------------------------------------------------------------------------
; Return 1 (true) if our value is 0, otherwise return 0 (false).
;
function BigInteger::_overloadTilde
  compile_opt idl2, hidden
  on_error, 2
  compare = self._compareTo(self, 0)
  return, compare eq 0
end


;---------------------------------------------------------------------------
function BigInteger::_overloadXOR, arg1, arg2
  compile_opt idl2, hidden
  on_error, 2

  self._bitWiseOperatorArgs, arg1, arg2, smaller, larger
  ; For the "XOR" operator, any extra digits in the larger integer will be kept.
  result = larger
  for i=0,N_ELEMENTS(smaller)-1 do begin
    result[i] = smaller[i] xor larger[i]
  endfor
  return, BigInteger(result)
end


;---------------------------------------------------------------------------
pro BigInteger__define
  compile_opt idl2, hidden
  void = {BigInteger, inherits IDL_Object, $
    isNegative: 0b, $
    isInfinite: 0b, $
    isNAN: 0b, $
    pDigits: PTR_NEW() $
    }
end
