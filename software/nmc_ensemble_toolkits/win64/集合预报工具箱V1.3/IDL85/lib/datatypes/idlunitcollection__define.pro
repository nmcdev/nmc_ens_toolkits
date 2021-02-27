; $Id: //depot/Release/ENVI53_IDL85/idl/idldir/lib/datatypes/idlunitcollection__define.pro#1 $
;
; Copyright (c) 2012-2015, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.
;
;+
; CLASS_NAME:
;    IDLUnitCollection
;
; PURPOSE:
;    The IDLUnitCollection class is used solely to define !UNIT, which
;   provides a central store for predefined (below) and user-defined
;   (at runtime) units, for use by the IDLUnit class.
;    IDL 8.3+ will automatically define !UNIT with an instance of this
;   class the first time it's accessed.
;
; CATEGORY:
;    Datatypes
;
;-


pro IDLUnitCollection::defineStandardUnits
  compile_opt IDL2, HIDDEN

  ; Can't do all this in ::init, because parsing depends on !UNIT already 
  ; existing.  Don't repeat all this work, and don't allow infinite loops 
  ; when the parser ensures its units exist.
  if self.standardsDefined then return
  self.standardsDefined = 1

  ; Length
  self.addUnit, 'micron',             '1e-6      meter',          PLURAL='microns'
  self.addUnit, 'inch',               '0.0254    meter',          PLURAL='inches',        SYMBOL='in'
  self.addUnit, 'point',              '(1/72)    inch',           PLURAL='points',        SYMBOL='pt'
  self.addUnit, 'pica',               '12        point',          PLURAL='picas'
  self.addUnit, 'foot',               '12        inch',           PLURAL='feet',          SYMBOL='ft'
  self.addUnit, 'yard',               '3         foot',           PLURAL='yards',         SYMBOL='yd'
  self.addUnit, 'mile',               '5280      foot',           PLURAL='miles',         SYMBOL='mi'
  self.addUnit, 'link',               '7.92      inch',           PLURAL='links'
  self.addUnit, 'rod',                '16.5      foot',           PLURAL='rods'
  self.addUnit, 'chain',              '4         rod',            PLURAL='chains'
  self.addUnit, 'furlong',            '10        chain',          PLURAL='furlongs'
  self.addUnit, 'league',             '3         mile',           PLURAL='leagues'
  self.addUnit, 'fathom',             '2         yard',           PLURAL='fathoms'
  self.addUnit, 'cable',              '120       fathom',         PLURAL='cables'
  self.addUnit, 'nautical_mile',      '1852      meter',          PLURAL='nautical_miles', SYMBOL='nmi'

  ; Area
  self.addUnit, 'acre',               '(1/640) mile^2',           PLURAL='acres',         SYMBOL='ac'
  self.addUnit, 'hectare',            '10000 meter^2',            PLURAL='hectares',      SYMBOL='ha'

  ; Volume
  self.addUnit, 'liter',              '(1/1000) meter^3',         PLURAL='liters',        SYMBOL='l', /PREFIXABLE, /PREFER 
  self.addUnit, 'fluid_ounce',        '29.573529563e-6 meter^3',  PLURAL='fluid_ounces',  SYMBOL='floz'
  self.addUnit, 'minim',              '(1/480) fluid_ounce',      PLURAL='minims'
  self.addUnit, 'fluid_dram',         '(1/8)  fluid_ounce',       PLURAL='fluid_drams'
  self.addUnit, 'teaspoon',           '(1/6)  fluid_ounce',       PLURAL='teaspoons',     SYMBOL='tsp'
  self.addUnit, 'tablespoon',         '3      teaspoon',          PLURAL='tablespoons',   SYMBOL='tbsp'
  self.addUnit, 'jigger',             '3      tablespoon',        PLURAL='jiggers'
  self.addUnit, 'gill',               '4      fluid_ounce',       PLURAL='gills'
  self.addUnit, 'cup',                '8      fluid_ounce',       PLURAL='cups'
  self.addUnit, 'pint',               '2      cup',               PLURAL='pints'
  self.addUnit, 'quart',              '2      pint',              PLURAL='quarts'
  self.addUnit, 'gallon',             '4      quart',             PLURAL='gallons',       SYMBOL='gal'
  self.addUnit, 'barrel',             '31.5   gallon',            PLURAL='barrels'
  self.addUnit, 'oil_barrel',         '42     gallon',            PLURAL='oil_barrels'
  self.addUnit, 'hogshead',           '63     gallon',            PLURAL='hogsheads'
  self.addUnit, 'dash',               '(1/8)  teaspoon',          PLURAL='dashes'
  self.addUnit, 'pinch',              '(1/16) teaspoon',          PLURAL='pinches'
  self.addUnit, 'smidgen',            '(1/32) teaspoon',          PLURAL='smidgens'

  ; Mass
  self.addUnit, 'pound',              '453.59237  gram',          PLURAL='pounds',        SYMBOL='lbs'
  self.addUnit, 'grain',              '(1/7000)   pound',         PLURAL='grains'
  self.addUnit, 'dram',               '(1/256)    pound',         PLURAL='drams'
  self.addUnit, 'ounce',              '(1/16)     pound',         PLURAL='ounces',        SYMBOL='oz'
  self.addUnit, 'hundredweight',      '100        pound',         PLURAL='hundredweights'
  self.addUnit, 'ton',                '2000       pound',         PLURAL='tons'

  ; Time
  self.addUnit, 'hertz',              '1   /   second',                                   SYMBOL='Hz',  /PREFIXABLE,  /PREFER
  self.addUnit, 'minute',             '60      second',           PLURAL='minutes',       SYMBOL='min',               /PREFER
  self.addUnit, 'hour',               '60      minute',           PLURAL='hours',                                     /PREFER
  self.addUnit, 'day',                '24      hour',             PLURAL='days',                                      /PREFER
  self.addUnit, 'week',               '7       day',              PLURAL='weeks'
  self.addUnit, 'fortnight',          '14      day',              PLURAL='fortnights'
  self.addUnit, 'year',               '365.25  day',              PLURAL='years',                                     /PREFER

  ; Velocity
  self.addUnit, 'kph',                '1000 meter / hour',                                SYMBOL='kph'       
  self.addUnit, 'mph',                '1 mile / hour',                                    SYMBOL='mph'
  self.addUnit, 'fps',                '1 foot / second',                                  SYMBOL='fps'

  ; Acceleration
  self.addUnit, 'gravity',            '9.80665 meter/second^2',   PLURAL='gravities', /PREFIXABLE, /PREFER
  self.addUnit, 'galileo',            '0.01 meter/second^2',      PLURAL='galileos',      SYMBOL='Gal'

  ; Force
  self.addUnit, 'newton',             '1000 meter*gram/second^2', PLURAL='newtons',       SYMBOL='N',   /PREFIXABLE, /PREFER
  self.addUnit, 'pound_force',        '1 gravity pound',          PLURAL='pounds_force',  SYMBOL='lbf'
  self.addUnit, 'dyne',               '1e-5 newton',                                      SYMBOL='dyn'

  ; Pressure
  self.addUnit, 'pascal',             '1 newton/meter^2',         PLURAL='pascals',       SYMBOL='Pa', /PREFIXABLE, /PREFER
  self.addUnit, 'psi',                '1 pound_force / inch^2',                           SYMBOL='psi'
  self.addUnit, 'bar',                '100000 pascal'
  self.addUnit, 'atmosphere',         '101325 pascal',            PLURAL='atmospheres',   SYMBOL='atm'
  self.addUnit, 'torr',               '133.3224 pascal'

  ; Data
  self.addUnit, 'byte',               '8 bit',                    PLURAL='bytes',         SYMBOL='B', /PREFIXABLE, /BINARY, /PREFER

  ; Electrical
  self.addUnit, 'amp',                '1 ampere',                 PLURAL='amps',                      /PREFIXABLE
  self.addUnit, 'watt',               '1000 meter^2 gram / second^3', PLURAL='watts',     SYMBOL='W', /PREFIXABLE, /PREFER
  self.addUnit, 'volt',               '1 watt / ampere',          PLURAL='volts',         SYMBOL='V', /PREFIXABLE, /PREFER
  self.addUnit, 'ohm',                '1 volt / ampere',          PLURAL='ohms',                      /PREFIXABLE, /PREFER
  self.addUnit, 'coulomb',            '1 ampere second',          PLURAL='coulombs',      SYMBOL='C', /PREFIXABLE, /PREFER
  self.addUnit, 'farad',              '1 coulomb / volt',         PLURAL='farads',        SYMBOL='F', /PREFIXABLE, /PREFER
  
  ; Energy
  self.addUnit, 'joule',              '1 newton meter',           PLURAL='joules',        SYMBOL='J', /PREFER
  self.addUnit, 'erg',                '1e-7 joule',               PLURAL='ergs'
  self.addUnit, 'calorie',            '4.184 watt second',        PLURAL='calories',      SYMBOL='cal'
  self.addUnit, 'british_thermal_unit', '1055 joule',                                     SYMBOL='BTU'
    
  ; Temperature
  self.addUnit, 'celsius',            '1     kelvin',   OFFSET=273.15,                    SYMBOL='deg_C'
  self.addUnit, 'fahrenheit',         '(5/9) kelvin',   OFFSET=459.67,                    SYMBOL='deg_F'

  ; Flux Density
  self.addUnit, 'jansky',             '1e-26 W/m^2/Hz',           PLURAL='janskies',      SYMBOL='Jy', /PREFER

  ; Constants
  self.addUnit, 'astronomical_unit',     '1.49597870700e11 meter',   PLURAL='astronomical_units',  SYMBOL='au'
  self.addUnit, 'speed_of_light',        '299792458 meter / second',                               SYMBOL='c'  ; In vacuum
  self.addUnit, 'elementary_charge',     '1.602176565e-19 coulomb',                                SYMBOL='e'
  self.addUnit, 'electric_permittivity', '8.854187817e-12 farad / meter',                          SYMBOL='eps0'
  self.addUnit, 'faraday',               '96485.3365 coulomb / mol',    PLURAL='faradays'
  self.addUnit, 'gravitation_constant',  '6.67384e-14 meter^3 / gram / second^2',                  SYMBOL='G'
  self.addUnit, 'planck_constant',       '6.62606957e-34 joule second',                            SYMBOL='h'
  self.addUnit, 'lightyear',             '9.460730472580800e15 meter',  PLURAL='lightyears',       SYMBOL='ly'
  self.addUnit, 'earth_mass',            '5.972186390e27 gram',                                    SYMBOL='M_earth'
  self.addUnit, 'sun_mass',              '1.98841586057e33 gram',                                  SYMBOL='M_sun'
  self.addUnit, 'electron_mass',         '9.10938291e-28 gram',                                    SYMBOL='me'
  self.addUnit, 'neutron_mass',          '1.674927351e-24 gram',                                   SYMBOL='mn'
  self.addUnit, 'proton_mass',           '1.672621777e-24 gram',                                   SYMBOL='mp'
  self.addUnit, 'magnetic_permeability', '12.566370614e-7 newton / ampere^2',                      SYMBOL='mu0'
  self.addUnit, 'Loschmidt_number',      '2.6867805e25 / meter^3',                                 SYMBOL='n0'
  self.addUnit, 'avogadro',              '6.02214129e23 / mole',                                   SYMBOL='Na'
  self.addUnit, 'parsec',                '3.0856775814671912e16 meter', PLURAL='parsecs',          SYMBOL='pc'
  self.addUnit, 'gas_constant',          '8.3144621 joule / mole / kelvin'
  self.addUnit, 'earth_radius',          '6378136.6 meter',                                        SYMBOL='R_earth'
  self.addUnit, 'electron_radius',       '2.8179403267e-15 meter',                                 SYMBOL='re'
  self.addUnit, 'rydberg',               '10973731.568539 / meter'
  self.addUnit, 'sigma',                 '5.670373e-8 watt / meter^2 / kelvin^4'
  self.addUnit, 'standard_temperature',  '273.15 kelvin',                                          SYMBOL='T0'
  self.addUnit, 'atomic_mass',           '1.660538921e-24 gram'
  self.addUnit, 'molar_volume',          '22.413968e-3  meter^3 / mole'
  
end

function IDLUnitCollection::_overloadPrint
  compile_opt IDL2, HIDDEN
  self.defineStandardUnits

  baseUnitNames = ['meter', 'gram', 'second', 'ampere', 'kelvin', 'mole', 'candela', 'bit']

  result = strarr(1, (self.list).Count() > 1)
  foreach unit, self.list, i do begin
    if strlen(unit.name) gt 0 then $
      result[i] = '1 ' + unit.name + ' = ' $
    else $
      result[i] = '1 <Unnamed> = '
    result[i] += string(unit.scale, FORMAT='(G0)')
    for j=0, n_elements(baseUnitNames)-1 do begin
      if unit.dim[j] ne 0 then begin
        result[i] += ' ' + baseUnitNames[j]
        if unit.dim[j] ne 1 then $
          result[i] += '^' + string(unit.dim[j], FORMAT='(G0)')
      endif
    endfor 
    if unit.offset ne 0 then begin
      result[i] += ' + ' + string(unit.offset, FORMAT='(G0)')
    endif
  endforeach

  return, result
end

function IDLUnitCollection::recognizeUnit, unitName
  compile_opt IDL2, HIDDEN
  self.defineStandardUnits

  ; Check if it's just a known unit or unit symbol
  ; First check full unit names
  if (self.names).HasKey(strlowcase(unitName)) then begin
    unit = (self.names)[strlowcase(unitName)]
    unit.displayName = unitName
    return, unit
  endif
  ; Then unit symbols
  if (self.symbols).HasKey(unitName) then begin
    unit = (self.symbols)[unitName]
    unit.displayName = unitName
    return, unit
  endif

  ; That didn't work.  Maybe it has a prefix.
  ; First full prefix names + full unit names
  foreach prefix, self.prefixes, prefix_name do begin
    if ~strcmp(strlowcase(unitName), prefix_name, strlen(prefix_name)) then continue
    suffix = strmid(strlowcase(unitName), strlen(prefix_name))
    if (self.names).HasKey(suffix) then begin
      unit = (self.names)[suffix]
      unit.displayName = suffix
      unit.prefix = prefix_name
      unit.prefixScale = (unit.binary ? 1024d : 1000d)^prefix  
      return, unit
    endif
  endforeach
  ; Now try prefix symbols + unit symbols
  foreach prefix, self.prefix_symbols, prefix_symbol do begin
    if ~strcmp(unitName, prefix_symbol, strlen(prefix_symbol)) then continue
    suffix = strmid(unitName, strlen(prefix_symbol))
    if (self.symbols).HasKey(suffix) then begin
      unit = (self.symbols)[suffix]
      ; Can this unit have prefixes?
      if ~unit.prefixable then continue
      ; We have a match!
      unit.displayName = suffix
      unit.prefix = prefix_symbol
      unit.prefixScale = (unit.binary ? 1024d : 1000d)^prefix  
      return, unit
    endif
  endforeach
  
  message, /NONAME, 'Unit "' + unitName + '" not recognized.'
end

pro IDLUnitCollection::addUnit, name, value, DIM=dim, OFFSET=offset, PLURAL=plural, SYMBOL=symbol, $
  PREFIXABLE=prefixable, BINARY=binary, PREFER=prefer
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error

  ; New struct with default values 
  newTerm = {IDLUNITTERM, scale:1, offset:0, dim:intarr(8), name:'', plural:'', symbol:'', $
    prefixable:0, binary:0, displayName:'', prefix:'', prefixScale:1, exponent:1}

  ; Validate input and copy to the struct as appropriate
  if isa(name, 'string') && strlen(name) gt 0 then begin
    newTerm.name = name
    newTerm.displayName = name
  endif else $
    message, /NONAME, "NAME must be a string.'

  if isa(plural, 'string') && strlen(plural) gt 0 then begin
    newTerm.plural = plural
  endif else $
  if plural ne !NULL then $
    message, /NONAME, "PLURAL must be a string.'

  if isa(symbol, 'string') && strlen(symbol) gt 0 then begin
    newTerm.symbol = symbol
  endif else $
  if symbol ne !NULL then $
    message, /NONAME, "SYMBOL must be a string.'

  if isa(value, /SCALAR, /NUMBER) then begin
    newTerm.scale = double(value)
  endif else $
  if isa(value, 'string') then begin
    parsed = IDLUNIT(value, SIMPLIFY=0)
    newTerm.scale  = parsed.quantity * parsed.scale
    newTerm.offset = parsed.offset
    newTerm.dim    = parsed.dim
  endif else $ 
    message, /NONAME, 'VALUE must be a string or number.'

  if isa(dim, /ARRAY, /NUMBER) || n_elements(dim) eq 8 then begin
    newTerm.dim = fix(dim) 
  endif else $
  if dim ne !NULL then $
    message, /NONAME, 'DIM must be an array of 8 numbers.'

  if isa(offset, /SCALAR, /NUMBER) then begin
    newTerm.offset = double(offset)
  endif else $
  if offset ne !NULL then $
    message, /NONAME, 'OFFSET must be a number.'

  newTerm.prefixable = keyword_set(prefixable)
  newTerm.binary     = keyword_set(binary)

  ; Make sure there are no conflicts with the existing units
  if (self.names).hasKey(name) then $
      message, /NONAME, 'Unit name ' + name + ' is already defined!'
  if plural ne !NULL && (self.names).hasKey(plural) then $
      message, /NONAME, 'Unit plural ' + plural + ' is already defined!'
  if symbol ne !NULL && (self.symbols).hasKey(symbol) then $
      message, /NONAME, 'Unit symbol ' + symbol + ' is already defined!'

  ; Finally, add it with the others
  (self.list).Add, newTerm
  (self.names)[strlowcase(name)] = newTerm

  if keyword_set(prefer) then $
    (self.preferred).Add, newTerm
  
  if plural ne !NULL then $
    (self.names)[strlowcase(plural)] = newTerm
  
  if symbol ne !NULL then $
    (self.symbols)[symbol] = newTerm
end

pro IDLUNITCOLLECTION::removeUnit, name
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error

  if not isa(name, 'string') then $
    message, /NONAME, 'NAME must be a string.'
  
  if not (self.names).HasKey(strlowcase(name)) then $
    message, /NONAME, 'Unit ' + name + ' is not defined.'
    
  unit = (self.names)[strlowcase(name)]
  (self.names).remove, strlowcase(unit.name)
  
  listIndex = (self.list).where(unit)
  if isa(listIndex) then (self.list).remove, listIndex
  
  listIndex = (self.preferred).where(unit)
  if isa(listIndex) then (self.preferred).remove, listIndex
    
  if strlen(unit.plural) gt 0 and (self.names).HasKey(unit.plural) then $
    (self.names).remove, unit.plural
  
  if strlen(unit.symbol) gt 0 and (self.symbols).HasKey(unit.symbol) then $
    (self.symbols).remove, unit.symbol
end

pro IDLUNITCOLLECTION::getProperty, LIST=list, PREFERRED=preferred, _REF_EXTRA=_extra
  compile_opt IDL2, HIDDEN
  on_error, 2 ; Return to caller on error 
  
  self.defineStandardUnits

  if arg_present(list)      then list = list(self.list, /EXTRACT)
  if arg_present(preferred) then preferred = list(self.preferred, /EXTRACT)

  catch, error
  if error ne 0 then begin
    catch, /CANCEL
    message, /NONAME, 'Unit not recognized.'
    return
  endif
  if _extra ne !NULL then begin
    foreach ex, _extra do $
      (scope_varfetch(ex, /REF_EXTRA)) = self.names[strlowcase(ex)]
  endif
  catch, /CANCEL
end


function IDLUNITCOLLECTION::init
  compile_opt IDL2, HIDDEN
  
  self.list            = list()
  self.preferred       = list()
  self.names           = hash()
  self.symbols         = hash()
  self.prefixes        = hash()
  self.prefix_symbols  = hash()

  ; Full prefixes, defined by kilo- orders of magnitude
  self.prefixes['yotta']   =  8
  self.prefixes['zetta']   =  7
  self.prefixes['exa']     =  6
  self.prefixes['peta']    =  5
  self.prefixes['tera']    =  4
  self.prefixes['giga']    =  3
  self.prefixes['mega']    =  2
  self.prefixes['kilo']    =  1
  self.prefixes['hecto']   =  2d/3
  self.prefixes['deca']    =  1d/3
  self.prefixes['deci']    = -1d/3
  self.prefixes['centi']   = -2d/3
  self.prefixes['milli']   = -1
  self.prefixes['micro']   = -2
  self.prefixes['nano']    = -3
  self.prefixes['pico']    = -4
  self.prefixes['femto']   = -5
  self.prefixes['atto']    = -6
  self.prefixes['zepto']   = -7
  self.prefixes['yocto']   = -8
  
  ; Prefix symbols
  self.prefix_symbols['Y']  =   8      ; yotta
  self.prefix_symbols['Z']  =   7      ; zetta
  self.prefix_symbols['E']  =   6      ; exa
  self.prefix_symbols['P']  =   5      ; peta
  self.prefix_symbols['T']  =   4      ; tera
  self.prefix_symbols['G']  =   3      ; giga
  self.prefix_symbols['M']  =   2      ; mega
  self.prefix_symbols['k']  =   1      ; kilo
  self.prefix_symbols['h']  =   2d/3   ; hecto
  self.prefix_symbols['da'] =   1d/3   ; deca
  self.prefix_symbols['d']  =  -1d/3   ; deci
  self.prefix_symbols['c']  =  -2d/3   ; centi
  self.prefix_symbols['m']  =  -1      ; milli
  self.prefix_symbols['u']  =  -2      ; micro
  self.prefix_symbols['n']  =  -3      ; nano
  self.prefix_symbols['p']  =  -4      ; pico
  self.prefix_symbols['f']  =  -5      ; femto
  self.prefix_symbols['a']  =  -6      ; atto
  self.prefix_symbols['z']  =  -7      ; zepto
  self.prefix_symbols['y']  =  -8      ; yocto

  ; Finally, define all the units!  
  ; First, all the fundamentals.
  self.addUnit, 'meter',   1, DIM=[1,0,0,0,0,0,0,0], PLURAL='meters',   SYMBOL='m',   /PREFIXABLE, /PREFER
  self.addUnit, 'gram',    1, DIM=[0,1,0,0,0,0,0,0], PLURAL='grams',    SYMBOL='g',   /PREFIXABLE, /PREFER
  self.addUnit, 'second',  1, DIM=[0,0,1,0,0,0,0,0], PLURAL='seconds',  SYMBOL='s',   /PREFIXABLE, /PREFER
  self.addUnit, 'ampere',  1, DIM=[0,0,0,1,0,0,0,0], PLURAL='amperes',  SYMBOL='A',   /PREFIXABLE, /PREFER
  self.addUnit, 'kelvin',  1, DIM=[0,0,0,0,1,0,0,0],                    SYMBOL='K',   /PREFIXABLE, /PREFER
  self.addUnit, 'mole',    1, DIM=[0,0,0,0,0,1,0,0], PLURAL='moles',    SYMBOL='mol', /PREFIXABLE, /PREFER
  self.addUnit, 'candela', 1, DIM=[0,0,0,0,0,0,1,0], PLURAL='candelas', SYMBOL='cd',  /PREFIXABLE, /PREFER
  self.addUnit, 'bit',     1, DIM=[0,0,0,0,0,0,0,1], PLURAL='bits',     SYMBOL='b',   /PREFIXABLE, /PREFER, /BINARY

  return, 1
end

pro IDLUNITCOLLECTION__DEFINE
  compile_opt IDL2, HIDDEN
  
  !NULL = {IDLUNITCOLLECTION, INHERITS IDL_Object, $
    list:           obj_new(), $
    preferred:      obj_new(), $
    names:          obj_new(), $
    plurals:        obj_new(), $
    symbols:        obj_new(), $
    prefixes:       obj_new(), $
    prefix_symbols: obj_new(), $
    standardsDefined: 0        }
  !NULL = {IDLUNITTERM, $
    scale:          1d, $
    offset:         0d, $
    dim:            intarr(8), $
    name:           '', $
    plural:         '', $
    symbol:         '', $
    prefixable:     0b, $
    binary:         0b, $
    displayName:    '', $
    prefix:         '', $
    prefixScale:    1d, $
    exponent:       1d  }
end

