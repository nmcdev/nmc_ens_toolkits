; $Id: //depot/Release/ENVI53_IDL85/idl/idldir/lib/datatypes/idlunit__define.pro#1 $
;
; Copyright (c) 2012-2015, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.
;
;+
; CLASS_NAME:
;    IDLUnit
;
; PURPOSE:
;    The IDLUnit class performs basic math and dimensional analysis on
;   string expressions with units, such as:
;
;   IDL> print, IDLUnit('12V / 20 milliamps')
;   600 ohm
;
; CATEGORY:
;    Datatypes
;
;-



;---------------------------------------------------------------------------
; This is a LL(1) parser, which implements the following (Antlr) grammar.
; If you plan on modifying this code, I strongly recommend you start with the
; grammar, validate it with Antlr, and only then modify the implementation to
; match the grammar.
;
;  options { k=1; }
;
;  stat     : addExpr (('->'|'to ') multExpr)? EOF ;
;       
;  addExpr  : multExpr (('+'|'-') multExpr)* ;
;   
;  multExpr : negExpr (('*'|'/') negExpr)* ;  
;
;  negExpr  : ('+'|'-')? impMultExpr ; 
;
;  impMultExpr : value value* ;
;  
;  value    :   FLOAT           exponent?
;           |   UNIT            exponent?
;           |   '(' addExpr ')' exponent? ;
;  
;  exponent : '^' ('+'|'-')? FLOAT ;


;  stat     : addExpr ('->' multExpr)? EOF ;
function IDLUnit::_parser_stat, tokens, value, outUnit
  compile_opt IDL2, HIDDEN

  if self->_parser_addExpr(tokens, value) then begin
    if isa(value, /number) then value = obj_new('IDLUNIT', value, SIMPLIFY=0)
    if ~value.hasQuantity then $
      value = obj_new('IDLUNIT', value, QUANTITY=1, SIMPLIFY=0)
    if self->_parser_tryToken(tokens, 'toOp') then begin
      if ~self->_parser_multExpr(tokens, outUnit) then return, 0
      if outUnit.hasQuantity then $
        message, /NONAME, 'Output unit must not have quantity!'
    endif
    if ~self->_parser_tryToken(tokens, 'end') then return, 0    
    return, 1
  endif

  return, 0
end


;---------------------------------------------------------------------------
;  addExpr  : multExpr (('+'|'-') multExpr)* ;
function IDLUnit::_parser_addExpr, tokens, value
  compile_opt IDL2, HIDDEN

  if self->_parser_multExpr(tokens, value) then begin
    while 1 do begin
      if self->_parser_tryToken(tokens, 'addOp') then begin
        if ~self->_parser_multExpr(tokens, rvalue) then return, 0
        value += rvalue
        continue
      endif
      if self->_parser_tryToken(tokens, 'subOp') then begin
        if ~self->_parser_multExpr(tokens, rvalue) then return, 0
        value -= rvalue
        continue
      endif
      break ; Only loop if one of the above cases was successful
    endwhile

    return, 1
  endif

  return, 0
end 


;---------------------------------------------------------------------------
;  multExpr : negExpr (('*'|'/') negExpr)* ;  
function IDLUnit::_parser_multExpr, tokens, value
  compile_opt IDL2, HIDDEN

  if self->_parser_negExpr(tokens, value) then begin
    while 1 do begin
      if self->_parser_tryToken(tokens, 'mulOp', token) then begin
        if ~self->_parser_negExpr(tokens, rvalue) then return, 0
        value = value._overloadAsterisk(value, rvalue, SIMPLIFY=0)
        continue
      endif
      if self->_parser_tryToken(tokens, 'divOp') then begin
        if ~self->_parser_negExpr(tokens, rvalue) then return, 0
        value = value._overloadSlash(value, rvalue, SIMPLIFY=0)
        continue
      endif
      break ; Only loop if one of the above cases was successful
    endwhile
    return, 1
  endif

  return, 0
end 


;---------------------------------------------------------------------------
;  negExpr  : ('+'|'-')? impMultExpr ; 
function IDLUnit::_parser_negExpr, tokens, value
  compile_opt IDL2, HIDDEN

  ; Negation
  if self->_parser_tryToken(tokens, 'subOp') then begin
    negate = 1
  endif else begin
    ; Superfluous posation- don't have to do anything
    negate = 0
    !NULL = self->_parser_tryToken(tokens, 'addOp')
  endelse 

  if ~self->_parser_impMultExpr(tokens, value) then return, 0
  
  if negate then $
    value = -value
  
  return, 1
end


;---------------------------------------------------------------------------
; impMultExpr : value value* ;
function IDLUnit::_parser_impMultExpr, tokens, value
  compile_opt IDL2, HIDDEN

  if self->_parser_value(tokens, value) then begin
    while self->_parser_value(tokens, rvalue) do begin
      value = value._overloadAsterisk(value, rvalue, SIMPLIFY=0)
    endwhile
    return, 1
  endif

  return, 0
end


;---------------------------------------------------------------------------
; value     :   FLOAT           exponent?
;           |   UNIT            exponent?
;           |   '(' addExpr ')' exponent? ;
function IDLUnit::_parser_value, tokens, value
  compile_opt IDL2, HIDDEN

  if self->_parser_tryToken(tokens, 'num', value) then begin
    value = double(value)
    if self->_parser_exponent(tokens, rvalue) then begin
      value ^= rvalue
    endif
    value = obj_new('IDLUnit', double(value), SIMPLIFY=0)
    return, 1
  endif
  
  if self->_parser_tryToken(tokens, 'str', value) then begin
    newUnit = !UNIT->recognizeUnit(value)
    value = obj_new('IDLUnit', UNIT=newUnit, SIMPLIFY=0)
    if self->_parser_exponent(tokens, rvalue) then begin
      value = value._overloadCaret(value, rvalue, SIMPLIFY=0)
    endif
    return, 1
  endif

  if self->_parser_tryToken(tokens, 'lParen') then begin
    if ~self->_parser_addExpr(tokens, value) then return, 0
    if ~self->_parser_tryToken(tokens, 'rParen') then return, 0
    if self->_parser_exponent(tokens, rvalue) then begin
      value = value._overloadCaret(value, rvalue, SIMPLIFY=0)
    endif
    return, 1
  endif
  
  return, 0
end


;---------------------------------------------------------------------------
;  exponent: '^' ('+'|'-')? value ;
function IDLUnit::_parser_exponent, tokens, value
  compile_opt IDL2, HIDDEN

  if self->_parser_tryToken(tokens, 'expOp') then begin
    ; Negation
    if self->_parser_tryToken(tokens, 'subOp') then begin
      negate = 1
    endif else begin
      ; Superfluous posation- don't have to do anything
      negate = 0
      !NULL = self->_parser_tryToken(tokens, 'addOp')
    endelse 
    
    if ~self->_parser_value(tokens, rvalue) then return, 0
    if isa(rvalue, 'IDLUNIT') then begin
      if rvalue.hasUnit then $
        message, /NONAME, 'Exponents must be dimensionless!'
      rvalue = rvalue.val
    endif
    value = rvalue
    if negate then $
      value = -value
    return, 1
  endif
  
  return, 0
end


;---------------------------------------------------------------------------
; Handles all the literal tokens
function IDLUnit::_parser_tryToken, tokens, expect, value
  compile_opt IDL2, HIDDEN

  if (tokens.count() lt 1)      then return, 0
  if (tokens[0].type ne expect) then return, 0
  token = tokens.remove(0)
  value = token.val
  return, 1
end


;---------------------------------------------------------------------------
; This is a lexical analyzer that implements the following (Antlr) grammar (plus operators):
;
;  FLOAT   : ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
;          |             '.' ('0'..'9')+ EXPONENT?
;          | ('0'..'9')+                 EXPONENT?
;          ;
;  
;  fragment EXPONENT 
;          :   ('e'|'E'|'d'|'D') ('+'|'-')? ('0'..'9')+ 
;          ;
;  
;  WS      :   (' '|'\t')+ {$channel=HIDDEN;} 
;          ;
;  
;  UNIT    :   ('a'..'z'|'A'..'Z'|'_')('0'..'9'|'a'..'z'|'A'..'Z'|'_')*
;          ;
function IDLUnit::_lexer, string
  compile_opt IDL2, HIDDEN

  input = string
  output = list()
  discard = list()
  offset = 0
  
  while (offset lt strlen(input)) do begin
    if self->_lexerRecognize(input, offset, discard, '[ ]+', '')              then continue
    if self->_lexerRecognize(input, offset, output,  $
      '(([0-9]+\.?[0-9]*)|(\.[0-9]+))([EeDd][+-]?[0-9]+)?', 'num')            then continue
    if self->_lexerRecognize(input, offset, output,  'to ', 'toOp')           then continue
    if self->_lexerRecognize(input, offset, output,  $
      '[a-zA-Z_][0-9a-zA-Z_]*', 'str')                                        then continue
    if self->_lexerRecognize(input, offset, output,  '->', 'toOp')            then continue
    if self->_lexerRecognize(input, offset, output,  '\+', 'addOp')           then continue
    if self->_lexerRecognize(input, offset, output,  '-',  'subOp')           then continue
    if self->_lexerRecognize(input, offset, output,  '\*', 'mulOp')           then continue
    if self->_lexerRecognize(input, offset, output,  '/',  'divOp')           then continue
    if self->_lexerRecognize(input, offset, output,  '\^', 'expOp')           then continue
    if self->_lexerRecognize(input, offset, output,  '\(', 'lParen')          then continue
    if self->_lexerRecognize(input, offset, output,  '\)', 'rParen')          then continue

    message, /NONAME, "Lexical error at character " + $ 
                      string(offset, FORMAT='(G0)')  + $
                      ' (string "' + strmid(input, offset) + '").'
    
    return, output
  endwhile
  
  output.add, {type:'end', val:'', offset:offset}

  return, output
end


;---------------------------------------------------------------------------
function IDLUnit::_lexerRecognize, input, offset, output, pattern, typeName
  compile_opt IDL2, HIDDEN

  if (stregex(strmid(input,offset), '^'+pattern, LENGTH=len) eq 0) then begin
    output.add, {type:typeName, val:strmid(input, offset, len), offset:offset}
    offset += len
    return, 1
  endif
  return, 0
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadPrint
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 

  result = ''
  
  if self.hasQuantity then $
    result += string(self.quantity, FORMAT='(G0)')

  if strlen(self.unit) gt 0 then $
    result += ' ' + self.unit

  return, result
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadImpliedPrint, varname
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error
  return, self->_overloadPrint()
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadPlus, arg1, arg2
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 

  if ~isa(arg1, 'IDLUNIT') || ~isa(arg2, 'IDLUNIT') then $
    message, 'Incompatible types!'

  ; Two values can only be added if their units are equal
  if ~array_equal(arg1.dim, arg2.dim) then $
    message, /NONAME, 'Both sides of addition must have the same units.'

  ; Addition without quantity doesn't make sense
  if ~arg1.hasQuantity || ~arg2.hasQuantity then $
    message, /NONAME, 'Both sides of addition must have quantity!'
  
  newQuantity = arg1.quantity+(arg2.quantity*arg2.scale/arg1.scale)
  
  return, obj_new('IDLUNIT', arg1, QUANTITY=newQuantity, SIMPLIFY=0)
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadMinus, arg1, arg2
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 

  if ~isa(arg1, 'IDLUNIT') || ~isa(arg2, 'IDLUNIT') then $
    message, 'Incompatible types!'

  ; Two values can only be added if their units are equal
  if ~array_equal(arg1.dim, arg2.dim) then $
    message, /NONAME, 'Both sides of subtraction must have the same units.'

  ; Addition without quantity doesn't make sense
  if ~arg1.hasQuantity || ~arg2.hasQuantity then $
    message, /NONAME, 'Both sides of subtraction must have quantity!'
  
  newQuantity = arg1.quantity-(arg2.quantity*arg2.scale/arg1.scale)
  
  return, obj_new('IDLUNIT', arg1, QUANTITY=newQuantity, SIMPLIFY=0)
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadAsterisk, arg1, arg2, SIMPLIFY=simplify
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 

  ; 123 * IDLUnit('1 mile') 
  if isa(arg1, /SCALAR, /NUMBER) then $
    return, obj_new('IDLUNIT', arg2, QUANTITY=arg1*arg2.quantity, SIMPLIFY=0)
    
  ; IDLUnit('1 mile') * 123
  if isa(arg2, /SCALAR, /NUMBER) then $
    return, obj_new('IDLUNIT', arg1, QUANTITY=arg1.quantity*arg2, SIMPLIFY=0)
  
  ; IDLUnit('1 m') * IDLUnit('3 ft')
  if isa(arg1, 'IDLUNIT') && isa(arg2, 'IDLUNIT') then begin
    if arg1.hasQuantity then begin
      if arg2.hasQuantity then quantity = arg1.quantity * arg2.quantity $
      else                     quantity = arg1.quantity
    endif else begin
      if arg2.hasQuantity then quantity = arg2.quantity $
      else                     quantity = !NULL
    endelse
    return, obj_new('IDLUNIT', QUANTITY=quantity, UNIT=arg1.terms+arg2.terms, SIMPLIFY=simplify)
  endif
  
  message, 'Incompatible types!'
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadSlash, arg1, arg2, SIMPLIFY=simplify
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 

  ; 123 / IDLUnit('1 mile') 
  if isa(arg1, /SCALAR, /NUMBER) then begin
    ; Have to make a new list of terms with negated exponents
    newTerms = list()
    foreach term, arg2.terms do begin
      term.exponent *= -1
      newTerms.Add, term
    endforeach
    return, obj_new('IDLUNIT', QUANTITY=arg1/arg2.quantity, UNIT=newTerms, SIMPLIFY=simplify)
  endif
    
  ; IDLUnit('1 mile') / 123
  if isa(arg2, /SCALAR, /NUMBER) then $
    return, obj_new('IDLUNIT', arg1, QUANTITY=arg1.quantity/arg2, SIMPLIFY=0)
  
  ; IDLUnit('1 furlong') / IDLUnit('1 fortnight')
  if isa(arg1, 'IDLUNIT') && isa(arg2, 'IDLUNIT') then begin
    if arg1.hasQuantity then begin
      if arg2.hasQuantity then quantity = arg1.quantity / arg2.quantity $
      else                     quantity = arg1.quantity
    endif else begin
      if arg2.hasQuantity then quantity = 1d / arg2.quantity $
      else                     quantity = !NULL
    endelse
    newTerms = arg1.terms
    foreach term, arg2.terms do begin
      term.exponent *= -1
      newTerms.Add, term
    endforeach
    return, obj_new('IDLUNIT', QUANTITY=quantity, UNIT=newTerms, SIMPLIFY=SIMPLIFY)
  endif
  
  message, 'Incompatible units!'
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadCaret, arg1, arg2, SIMPLIFY=simplify
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 

  if ~isa(arg2, /SCALAR, /NUMBER) then $
    message, 'Incompatible units!'

  if arg1.hasQuantity then $
    quantity = arg1.quantity^arg2
  newTerms = list()
  foreach term, arg1.terms do begin
    term.exponent *= arg2
    newTerms.Add, term
  endforeach

  return, obj_new('IDLUNIT', QUANTITY=quantity, UNIT=newTerms, SIMPLIFY=simplify)
end


;---------------------------------------------------------------------------
function IDLUNIT::_overloadMinusUnary
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 
  
  if ~self.hasQuantity then $
    message, /NONAME, 'Expression must have quantity to negate.'
  
  return, obj_new('IDLUNIT', self, QUANTITY=-self.quantity, SIMPLIFY=0)
end


;---------------------------------------------------------------------------
function IDLUNIT::to, unitString
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 

  return, obj_new('IDLUNIT', self, TO=unitString)
end


;---------------------------------------------------------------------------
pro IDLUNIT::GetProperty, VAL=val, QUANTITY=quantity, UNIT=unit, TERMS=terms, $
  HASQUANTITY=hasQuantity, HASUNIT=hasUnit, SCALE=scale, DIM=dim, OFFSET=offset, $
  _REF_EXTRA=_extra
  compile_opt IDL2, HIDDEN, STATIC
  on_error, 2 ; Return to caller on error 

  ; If we're being called statically, then need to behave a little differently
  if not isa(self) then begin
    ; Ensure !UNIT is initialized
    DEFSYSV, '!UNIT', EXISTS=exists
    if (~exists) then DEFSYSV, '!UNIT', obj_new('IDLUnitCollection')

    if _extra ne !NULL then begin
      foreach ex, _extra do begin
        newUnit = !UNIT.recognizeUnit(strlowcase(ex))
        (scope_varfetch(ex, /REF_EXTRA)) = obj_new('IDLUnit', UNIT=newUnit, QUANTITY=1, SIMPLIFY=0)        
      endforeach
    endif
    return
  endif

  if arg_present(val)             then val            = self.quantity
  if arg_present(quantity)        then quantity       = self.quantity
  if arg_present(hasQuantity)     then hasQuantity    = self.hasQuantity
  if arg_present(hasUnit)         then hasUnit        = (self.terms).Count() gt 0
  ; For terms, return a copy so the callee can't modify our data!
  if arg_present(terms)           then terms          = list(self.terms, /EXTRACT)
  if arg_present(unit)            then unit           = self.unit
  if arg_present(scale)           then scale          = self.scale
  if arg_present(dim)             then dim            = self.dim
  if arg_present(offset)          then offset         = self.offset
end 


;---------------------------------------------------------------------------
function IDLUNIT::Init, value, QUANTITY=quantity, UNIT=unit, TO=toUnit, $
  SIMPLIFY=simplify, DECOMPOSE=decompose
  compile_opt IDL2, HIDDEN

  on_error, 2 ; Return to caller on error 
  catch, error
  if error ne 0 then begin
    catch, /CANCEL
    message, /NONAME, 'IDLUnit error: ' + !ERROR_STATE.msg
    return, 0
  endif

  ; Default values
  self.quantity       = 1
  self.hasQuantity    = 0
  self.terms          = list()
  
  if toUnit ne !NULL && keyword_set(simplify) then $
    message, /NONAME, 'TO and SIMPLIFY are mutually exclusive.'
  if toUnit ne !NULL && keyword_set(decompose) then $
    message, /NONAME, 'TO and DECOMPOSE are mutually exclusive.'
  if keyword_set(simplify) && keyword_set(decompose) then $
    message, /NONAME, 'SIMPLIFY and DECOMPOSE are mutually exclusive.'
  
  DEFSYSV, '!UNIT', EXISTS=exists
  if (~exists) then DEFSYSV, '!UNIT', obj_new('IDLUnitCollection') 
  
  ; SIMPLIFY:
  ;   0: Do not simplify
  ;   1: Cancel out terms
  ;  *2: Choose new unit, if better
  ;   3: Choose new unit
  if simplify eq !NULL then $
    simplify = 2
  
  if isa(value, /number, /scalar) then begin
    self.quantity = double(value)
    self.hasQuantity = 1
  endif else $
  if isa(value, 'IDLUNIT', /scalar) then begin
    ; Make myself into a copy of it
    self.quantity     = value.quantity
    self.hasQuantity  = value.hasQuantity
    self.terms        = value.terms
  endif else $
  if isa(value, 'string', /scalar) then begin
    ; Need to parse
    lexed = self->_lexer(value)  ; Token stream, will be consumed incrementally by the parser
    if ~self->_parser_stat(lexed, parsed, toUnit) then $
      message, /NONAME, 'Parsing error at character ' + $ 
                        string(lexed[0].offset, FORMAT='(G0)') + $
                        ' (token "' + lexed[0].val + '").' 
    self.quantity     = parsed.quantity
    self.hasQuantity  = parsed.hasQuantity
    self.terms        = parsed.terms
  endif else $
  if value ne !NULL then $  ; VALUE must be something unexpected
    message, /NONAME, 'The VALUE keyword must be a scalar number, IDLUnit object, or string.'
  
  ; Set individual properties
  if quantity ne !NULL then begin
    if ~isa(quantity, /number, /scalar) then $
      message, /NONAME, 'QUANTITY must be a scalar number.'
    self.quantity = double(quantity)
    self.hasQuantity = 1
  endif
  
  if isa(unit, 'IDLUNIT') then begin
    (self.terms).Add, unit.terms, /EXTRACT
  endif else $
  if isa(unit, 'IDLUNITTERM') then begin
    (self.terms).Add, unit
  endif else $
  if isa(unit, 'LIST') then begin
    foreach term, unit do $
      if isa(term, 'IDLUNITTERM') then (self.terms).Add, term
  endif else $
  if unit ne !NULL then $
    message, /NONAME, 'UNIT must be a IDLUnit object, IDLUnitTerm structure, or list of IDLUnitTerm structures.'

  ; Try to cancel out as many terms as possible
  if simplify ge 1 then begin
    oldTerms    = self.terms
    newTerms    = list()
    newQuantity = self.quantity
    while oldTerms.Count() gt 0 do begin
      term = oldTerms.Remove(0)
      ; Walk through all the other terms and see if there are any dimension matches
      foreach otherTerm, oldTerms, i do begin
        if array_equal(term.dim, otherTerm.dim) then begin     ; s * s^2 = s^3
          newQuantity *= ((otherTerm.scale * otherTerm.prefixScale) / $
                          (     term.scale *      term.prefixScale)) ^ otherTerm.exponent
          term.exponent += otherTerm.exponent
          oldTerms.Remove, i--
        endif else $
        if array_equal(term.dim, -otherTerm.dim) then begin    ; s * Hz^2 = s^-1
          term.exponent -= otherTerm.exponent
          newQuantity /= ((otherTerm.scale * otherTerm.prefixScale) / $
                          (     term.scale *      term.prefixScale)) ^ otherTerm.exponent
          oldTerms.Remove, i--
        endif
      endforeach
      if term.exponent ne 0 then newTerms.Add, term
    endwhile
    
    self.terms = newTerms
    self.quantity  = newQuantity
  endif

  ; Total up the unit terms' scales and dimensions.
  ; Useful for the simplifcation operations to follow.
  self.scale  = 1
  self.offset = 0
  self.dim    = intarr(8)
  totalTerms  = 0
  foreach term, self.terms do begin
    self.scale  *= (term.scale * term.prefixScale) ^ term.exponent
    self.offset += term.offset * term.scale
    self.dim    += term.dim * term.exponent
    totalTerms  += abs(term.exponent)
  endforeach
  totalScale  = self.quantity * self.scale 
  
  ; Offsets cannot coexist with other terms or exponents, so drop them.
  if self.offset ne 0 && totalTerms gt 1 then begin
    for i=0, n_elements(self.terms)-1 do begin
      temp = self.terms[i] 
      temp.offset = 0
      self.terms[i] = temp
    endfor
    self.offset = 0
  endif
  
  ; Check if the dimensions simplify to nothing.  Then we can skip a lot of work. 
  if totalTerms gt 0 && array_equal(self.dim, intarr(8)) then begin
    self.quantity *= self.scale
    (self.terms).Remove, /ALL
    simplify = 0
  endif

  ; If SIMPLIFY>=2, is there a significantly better unit we could be using?
  if simplify ge 2 && toUnit eq !NULL then begin
    ; Most of all, we want to reduce the number of terms.
    ; We'll be counting a term with exponent n as abs(n) terms.
    ; Our secondary goal is to minimize quantity, as long as it's at least 1.0
    bestScore = !VALUES.D_INFINITY
    bestUnit  = !NULL
    bestExponent = 0
    foreach niceUnit, !UNIT.preferred do begin
      niceUnitExponent = 1
      niceUnitScore = !VALUES.D_INFINITY
  
      ; Increase the exponent so long as it improves the score
      exponent = 0
      while 1 do begin
        exponent++
        score = exponent + 2*total(abs(self.dim - niceUnit.dim*exponent))
        if score le niceUnitScore then begin
          niceUnitExponent = exponent
          niceUnitScore = score
        endif else $
          break
      endwhile
      ; Can we do any better by walking into the negatives instead?
      exponent = 0
      while 1 do begin
        exponent--
        score = -exponent + 2*total(abs(self.dim - niceUnit.dim*exponent))
        if score le niceUnitScore then begin
          niceUnitExponent = exponent
          niceUnitScore = score
        endif else $
          break
      endwhile
      
      ; If this is better than any we've seen before, remember it
      if (niceUnitScore lt bestScore)      || $
         (  (niceUnitScore  eq bestScore)  && $
            (niceUnit.scale lt totalScale) && $
            (niceUnit.scale gt bestUnit.scale)) then begin
        bestScore     = niceUnitScore
        bestUnit      = niceUnit
        bestExponent  = niceUnitExponent
      endif
    endforeach

    ; Success is defined by reducing the number of terms by at least 1.
    if simplify ge 3 || bestScore le totalTerms-1 then begin
      ; Pick a prefix that results in the smallest quantity greater than 1
      prefix      = ''
      prefixScale = 1
      magnitude   = 1
      order       = 1
      if bestUnit.prefixable then begin
        ; Try and pick a prefix, using a more limited set than for input
        ; Since these are unchanging, no need to refer to !UNIT.
        ; The fudge factor is necessary because sometimes alog gives values
        ; infinitesimally below the floor.
        magnitude = bestUnit.binary ? 1024d : 1000d
        order = floor(alog(abs(totalScale/bestUnit.scale))/alog(magnitude)/bestExponent + 0.0000001) 
        case order of
          5:  prefix = 'peta'
          4:  prefix = 'tera'
          3:  prefix = 'giga'
          2:  prefix = 'mega'
          1:  prefix = 'kilo'
          -1: prefix = 'milli'
          -2: prefix = 'micro'
          -3: prefix = 'nano'
          -4: prefix = 'pico'
          -5: prefix = 'femto'
          else: order = 0
        endcase
      endif
      toUnit = bestUnit
      toUnit.prefix       = prefix
      toUnit.prefixScale  = magnitude^order
      toUnit.exponent     = bestExponent 
    endif
  endif

  ; Are we being asked to change the units?
  if toUnit ne !NULL || keyword_set(decompose) then begin
    ; Decompose by converting to nothing and letting the remainder code sort it out
    if keyword_set(decompose) then $
      toUnit = !NULL
    ; If we're converting to a specific new unit, do that
    if isa(toUnit, 'IDLUNIT') then begin
      if toUnit.hasQuantity then $
        message, /NONAME, 'Destination unit must not have quantity.'
      newTerms = toUnit.terms
    endif else $
    if isa(toUnit, 'IDLUNITTERM') then begin
      newTerms = list(toUnit)
    endif else $
    if isa(toUnit, 'string') then begin
      lexed = self->_lexer(toUnit)  ; Token stream, will be consumed incrementally by the parser
      if ~self->_parser_multExpr(lexed, newUnit) then $ 
        message, /NONAME, 'Parsing error at character ' + $ 
                          string(lexed[0].offset, FORMAT='(G0)') + $
                          ' (token "' + lexed[0].val + '").' 
      if newUnit.hasQuantity then $
        message, /NONAME, 'Destination unit must not have quantity!'
      newTerms = newUnit.terms
    endif else $
    if toUnit ne !NULL then begin
      message, /NONAME, 'TOUNIT must be a string, IDLUnit object or IDLUnitTerm structure.'
    endif else $
      newTerms = list()

    ; Divide the new terms out of the remainder
    totalScale += self.offset
    totalDim    = self.dim
    foreach term, newTerms do begin
      totalScale /= (term.scale * term.prefixScale) ^ term.exponent
      totalScale -= term.offset
      totalDim   -= term.dim * term.exponent
    endforeach
    
    ; The rest of the remainder has to go back as the base units
    baseUnits = [!UNIT.meter,  !UNIT.gram, !UNIT.second,  !UNIT.ampere, $ 
                 !UNIT.kelvin, !UNIT.mole, !UNIT.candela, !UNIT.bit]
    for i=0, n_elements(baseUnits)-1 do begin
      if totalDim[i] eq 0 then continue
      newTerm = baseUnits[i]
      newTerm.exponent = totalDim[i]
      newTerms.Add, newTerm 
    endfor

    ; We now have a new set of unit terms consistent with the old ones.  Swap them in.
    self.terms = newTerms
    self.quantity  = totalScale
    
    ; After all that work, no need to simplify anymore
    simplify = 0
  endif
  
  if ~self.hasQuantity && self.terms.Count() eq 0 then $
    message, /NONAME, 'IDLUnit object has neither quantity nor units!'
    
  ; Make a string to represent all the unit terms
  self.unit = ''
  ; Terms with positive exponents first
  foreach term, self.terms do begin
    if term.exponent gt 0 then begin
      if strlen(self.unit) gt 0 then $
        self.unit += ' '
      self.unit += term.prefix + term.displayName
      if term.exponent ne 1 then $
        self.unit += '^' + string(term.exponent, FORMAT='(G0)')
    endif
  endforeach
  ; Now terms with negative exponents
  foreach term, self.terms do begin
    if term.exponent lt 0 then begin
      if strlen(self.unit) gt 0 then $
        self.unit += ' '
      self.unit += '/ ' + term.prefix + term.displayName
      if term.exponent ne -1 then $
        self.unit += '^' + string(-term.exponent, FORMAT='(G0)')
    endif
  endforeach
  
    
  return, 1
end


;---------------------------------------------------------------------------
function IDLUNIT::ListUnits
  compile_opt IDL2, HIDDEN, STATIC
  on_error, 2

  ; Ensure !UNIT is initialized
  DEFSYSV, '!UNIT', EXISTS=exists
  if (~exists) then DEFSYSV, '!UNIT', obj_new('IDLUnitCollection')

  allUnits = !UNIT.list
  nameList = strarr(allUnits.count())
  for i=0, allUnits.count()-1 do nameList[i]=allUnits[i].name
  
  return, nameList
end


;---------------------------------------------------------------------------
pro IDLUNIT::AddUnit, name, value, _REF_EXTRA=ex
  compile_opt IDL2, HIDDEN, STATIC
  on_error, 2
  ; Ensure !UNIT is initialized
  DEFSYSV, '!UNIT', EXISTS=exists
  if (~exists) then DEFSYSV, '!UNIT', obj_new('IDLUnitCollection')

  !UNIT.addUnit, name, value, _EXTRA=ex
end


;---------------------------------------------------------------------------
pro IDLUNIT::RemoveUnit, name
  compile_opt IDL2, HIDDEN, STATIC
  on_error, 2
  
  ; Ensure !UNIT is initialized
  DEFSYSV, '!UNIT', EXISTS=exists
  if (~exists) then DEFSYSV, '!UNIT', obj_new('IDLUnitCollection')
  
  !UNIT.removeUnit, name
end


;---------------------------------------------------------------------------
pro IDLUNIT__DEFINE
  compile_opt IDL2, HIDDEN
  !NULL = {IDLUNIT, INHERITS IDL_Object, $
    quantity:       0d,         $   ; Base quantity
    hasQuantity:    0b,         $   ; Units have scales, but not quantities.
    scale:          1d,         $
    offset:         0d,         $
    dim:            intarr(8),  $
    unit:           '',         $
    terms:          list()      }   ; Array of unit terms
end