; $Id: //depot/Release/ENVI53_IDL85/idl/idldir/lib/datatypes/idl_convert.pro#1 $
;
; Copyright (c) 2012-2015, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.
;
;+
; NAME:
;    IDL_Convert
;
; PURPOSE:
;    A GUI converter using IDLUnit
;
; CATEGORY:
;    Macros
;
; MODIFICATION HISTORY:
;     Written by:   AGEH, 11/2012
;-

pro idl_convert_updatevalue, wFromVal, wToVal, wFrom, wTo
  compile_opt idl2, hidden

  catch, err
  if (err ne 0) then begin
    catch, /cancel
    message, 'Unable to convert '+str, /NONAME, /NOPREFIX
    return
  endif
  
  ; Get values from widgets
  from = WIDGET_INFO(wFrom, /COMBOBOX_GETTEXT)
  to = WIDGET_INFO(wTo, /COMBOBOX_GETTEXT)
  WIDGET_CONTROL, wFromVal, GET_VALUE=fromVal
  fromVal = fromVal[0]

  ; Ensure input value is valid
  on_ioerror,badVal
  fromVal = DOUBLE(fromVal)
  if (0) then begin
    badVal: fromVal = '0'
  endif
  
  ; Do the conversion
  if (fromVal eq '') then fromVal = '0'
  str = STRTRIM(fromVal, 2)+' '+from+' -> '+to
  dimAna = OBJ_NEW('IDLUnit',str)
  val = dimAna.val
  unit = dimAna.unit
  OBJ_DESTROY, dimAna
  ; Check output unit
  if (STRJOIN(STRSPLIT(STRLOWCASE(unit),' ',/EXTRACT)) ne $
      STRLOWCASE(to)) then !NULL/=0
  WIDGET_CONTROL, wToVal, SET_VALUE=STRING(val, FORMAT='(g0)')
  
end

pro idl_convert_event, ev
  compile_opt idl2, hidden
  
  WIDGET_CONTROL, ev.top, GET_UVALUE=pState

  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    WIDGET_CONTROL, (*pState).wStatus, SET_VALUE=!ERROR_STATE.MSG
    return
  endif else begin
    WIDGET_CONTROL, (*pState).wStatus, SET_VALUE=' '
  endelse

  uname = WIDGET_INFO(ev.id, /UNAME)
  case uname of
    'types' : begin
      WIDGET_CONTROL, (*pState).wLeft, $
        SET_VALUE=*((*pState).unitArray)[ev.index]
      WIDGET_CONTROL, (*pState).wRight, $
        SET_VALUE=*((*pState).unitArray)[ev.index]
      WIDGET_CONTROL, (*pState).wRight, SET_COMBOBOX_SELECT=1
      WIDGET_CONTROL, (*PState).wLeftVal, SET_VALUE='0'
      WIDGET_CONTROL, (*PState).wRightVal, SET_VALUE='0'
      idl_convert_updatevalue, (*pState).wLeftVal, (*pState).wRightVal, $
        (*pState).wLeft, (*pState).wRight
      WIDGET_CONTROL, (*pState).wLeftVal, /INPUT_FOCUS
      WIDGET_CONTROL, (*pState).wLeftVal, SET_TEXT_SELECT=[0,1000]
    end
    'left' : begin
      idl_convert_updatevalue, (*pState).wLeftVal, (*pState).wRightVal, $
        (*pState).wLeft, (*pState).wRight
      WIDGET_CONTROL, (*pState).wLeftVal, /INPUT_FOCUS
      WIDGET_CONTROL, (*pState).wLeftVal, SET_TEXT_SELECT=[0,1000]
    end
    'right' : begin
      idl_convert_updatevalue, (*pState).wLeftVal, (*pState).wRightVal, $
        (*pState).wLeft, (*pState).wRight
      WIDGET_CONTROL, (*pState).wRightVal, /INPUT_FOCUS
      WIDGET_CONTROL, (*pState).wRightVal, SET_TEXT_SELECT=[0,1000]
    end
    'valueleft' : begin
      idl_convert_updatevalue, (*pState).wLeftVal, (*pState).wRightVal, $
        (*pState).wLeft, (*pState).wRight
    end
    'valueright' : begin
      idl_convert_updatevalue, (*pState).wRightVal, (*pState).wLeftVal, $
        (*pState).wRight, (*pState).wLeft
    end
    else:
  endcase

end

pro idl_convert
  compile_opt idl2, hidden
  
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    !NULL = DIALOG_MESSAGE('Could not create converter')
    return
  endif
  
  ; Define units
  dim = hash()
  dim['length']        = [ 1, 0, 0, 0, 0, 0, 0, 0]
  dim['area']          = [ 2, 0, 0, 0, 0, 0, 0, 0]
  dim['volume']        = [ 3, 0, 0, 0, 0, 0, 0, 0]
  dim['mass']          = [ 0, 1, 0, 0, 0, 0, 0, 0]
  dim['time']          = [ 0, 0, 1, 0, 0, 0, 0, 0]
  dim['frequency']     = [ 0, 0,-1, 0, 0, 0, 0, 0]
  dim['force']         = [ 1, 1,-2, 0, 0, 0, 0, 0]
  dim['pressure']      = [-1, 1,-2, 0, 0, 0, 0, 0]
  dim['velocity']      = [ 1, 0,-1, 0, 0, 0, 0, 0]
  dim['acceleration']  = [ 1, 0,-2, 0, 0, 0, 0, 0]
  dim['current']       = [ 0, 0, 0, 1, 0, 0, 0, 0]
  dim['potential']     = [ 2, 1,-3,-1, 0, 0, 0, 0]
  dim['resistance']    = [ 2, 1,-3,-2, 0, 0, 0, 0]
  dim['charge']        = [ 0, 0, 1, 1, 0, 0, 0, 0]
  dim['capacitance']   = [-2,-1, 4, 2, 0, 0, 0, 0]
  dim['energy']        = [ 2, 1,-2, 0, 0, 0, 0, 0]
  dim['power']         = [ 2, 1,-3, 0, 0, 0, 0, 0]
  dim['temperature']   = [ 0, 0, 0, 0, 1, 0, 0, 0]
  dim['luminance']     = [ 0, 0, 0, 0, 0, 0, 1, 0]
  dim['data']          = [ 0, 0, 0, 0, 0, 0, 0, 1]

  ; Before touching !UNIT, we should be sure that it's initialized.
  ; IDLUnit will initialize it for us if it needs to be.
  !NULL = IDLUnit('1')

  ; Set up arrays of types and names
  typeNames = (dim.keys()).toarray(TYPE=7)
  typeValues = TRANSPOSE((dim.values()).toarray(TYPE=2))
  unitArray = PTRARR(dim.count())
  n =!unit.list.count()
  for i=0,n-1 do begin
    curDim = (!unit.list)[i].dim
    for j=0,N_ELEMENTS(typeNames)-1 do begin
      if (ARRAY_EQUAL(curDim, typeValues[*,j])) then begin
        if (~PTR_VALID(unitArray[j])) then begin
          unitArray[j] = PTR_NEW((!unit.list)[i].name)
        endif else begin
          *unitArray[j] = [*unitArray[j], (!unit.list)[i].name]
        endelse
      endif
    endfor
  endfor
  ; Customize units
  ; Exclude certain units
  exclude = ['ampere']
  for i=0,N_ELEMENTS(exclude)-1 do begin
    for j=0,N_ELEMENTS(unitArray)-1 do begin
      if (PTR_VALID(unitArray[j])) then begin
        !NULL = where(exclude[i] eq *unitArray[j], cnt, $
                      COMPLEMENT=wh, NCOMPLEMENT=ncnt)
        if ((cnt ne 0) && (ncnt ne 0)) then begin
          *unitArray[j] = (*unitArray[j])[wh]
        endif
      endif
    endfor
  endfor
  ; Add other units
  add = hash()
  add['area'] = ['m^2','km^2','mile^2','yard^2','foot^2','inch^2']
  add['data'] = ['kilobit','kilobyte','megabit','megabyte','gigabit', $
                 'gigabyte','terabit','terabyte','petabit','petabyte']
  add['acceleration'] = ['m/s^2','cm/s^2','ft/s^2']
  add['power'] = ['BTU/s','BTU/min','BTU/hour']
  add['velocity'] = ['m/s']
  add['mass'] = ['kilogram']
  add['volume'] = ['milliliter']
  add['length'] = ['kilometer','centimeter','millimeter','micrometer']
  keys = add.keys()
  for i=0,N_ELEMENTS(keys)-1 do begin
    wh = where(keys[i] eq typeNames, cnt)
    if (cnt eq 0) then continue
    newUnits = add[keys[i]]
    if (PTR_VALID(unitArray[wh[0]])) then begin
      *unitArray[wh[0]] = [*unitArray[wh[0]], newUnits]
    endif else begin
      unitArray[wh[0]] = PTR_NEW(newUnits)
    endelse
  endfor
  
  ; Filter out any types that do not have units
  wh = WHERE(PTR_VALID(unitArray), NCOMPLEMENT=cnt)
  if (cnt ne 0) then begin
    unitArray = unitArray[wh]
    typeNames = typeNames[wh]
  endif
  ; Filter out any types that only have one unit
  keep = [!NULL]
  for i=0,N_ELEMENTS(unitArray)-1 do begin
    if (N_ELEMENTS(*unitArray[i]) gt 1) then keep = [keep,i]
  endfor
  if (N_ELEMENTS(keep) ne 0) then begin
    unitArray = unitArray[keep]
    typeNames = typeNames[keep]
  endif
  ; Alphabatize types
  sort = sort(STRLOWCASE(typeNames))
  typeNames = typeNames[sort]
  unitArray = unitArray[sort]
  ; Alphabatize lists
  for i=0,N_ELEMENTS(unitArray)-1 do begin
    *unitArray[i] = (*unitArray[i])[SORT(STRLOWCASE(*unitArray[i]))]
  endfor
  
  ; Determine which fonts to use
  if (!VERSION.OS_FAMILY eq 'Windows') then begin
    fontLg = 'MS Shell Dlg*-24'
    fontSm = 'MS Shell Dlg*-20'
    tipStr = 'Tip: you may type custom units in either unit box, e.g.: ' + $
             'attoparsec or decifoot'
  endif else begin
    fontLg = WIDGET_INFO(WIDGET_LABEL(WIDGET_BASE(),VALUE='test'), /FONTNAME)
    fontSm = fontLg
    tipStr = 'Tip: you may type custom units in either unit box'
  endelse

  ; Create widget
  tlb = WIDGET_BASE(/COLUMN)
  
  wRow = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER)
  wLabel = WIDGET_LABEL(wRow, VALUE='IDL Converter', FONT=fontLg)

  wRow = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER)
  wLabel = WIDGET_LABEL(wRow, VALUE='Unit type: ', FONT=fontSm)
  wType = WIDGET_DROPLIST(wRow, VALUE=typeNames, UNAME='types', FONT=fontSm)
  geo = WIDGET_INFO(wType, /GEOMETRY)
  widget_control, wRow, SCR_YSIZE=geo.scr_ysize+16
  
  wBase = WIDGET_BASE(tlb, /COLUMN, SPACE=0)
  wRow = WIDGET_BASE(wBase, /ROW, YPAD=0)
  wLeftVal = WIDGET_TEXT(wRow, VALUE='0', EDITABLE=1, UNAME='valueleft', $
    /ALL_EVENTS, FONT=fontSm)
  wLabel1 = WIDGET_LABEL(wRow, VALUE='=', FONT=fontSm)
  wRightVal = WIDGET_TEXT(wRow, VALUE='0', EDITABLE=1, UNAME='valueright', $
    /ALL_EVENTS, FONT=fontSm)

  wRow = WIDGET_BASE(wBase, /ROW, YPAD=0)
  wLeft = WIDGET_COMBOBOX(wRow, VALUE=*unitArray[0], UNAME='left', /EDITABLE, $
    FONT=fontSm)
  wLabel2 = WIDGET_LABEL(wRow, VALUE=' ', FONT=fontSm)
  wRight = WIDGET_COMBOBOX(wRow, VALUE=*unitArray[0], UNAME='right', $
    /EDITABLE, FONT=fontSm)
  WIDGET_CONTROL, wRight, SET_COMBOBOX_SELECT=1
  
  wStatus = WIDGET_LABEL(wBase, VALUE=tipStr)
  
  ; Make things the same size
  geoLabel1 = WIDGET_INFO(wLabel1, /GEOMETRY)
  geoLabel2 = WIDGET_INFO(wLabel2, /GEOMETRY)
  scrxsize = geoLabel1.scr_xsize > geoLabel2.scr_xsize
  WIDGET_CONTROL, wLabel1, SCR_XSIZE=scrxsize
  WIDGET_CONTROL, wLabel2, SCR_XSIZE=scrxsize
  geoLeft = WIDGET_INFO(wLeftVal, /GEOMETRY)
  geoRight = WIDGET_INFO(wRightVal, /GEOMETRY)
  WIDGET_CONTROL, wLeft, SCR_XSIZE=geoLeft.scr_xsize
  WIDGET_CONTROL, wRight, SCR_XSIZE=geoRight.scr_xsize
  WIDGET_CONTROL, wStatus, SCR_XSIZE=geoLeft.scr_xsize + geoRight.scr_xsize
  
  ; Set nice starting values
  wh = where(typeNames eq 'length', cnt)
  if (cnt ne 0) then begin
    WIDGET_CONTROL, wType, SET_DROPLIST_SELECT=wh
    ind1 = where(*unitArray[wh[0]] eq 'foot', cnt1)
    ind2 = where(*unitArray[wh[0]] eq 'meter', cnt2)
    if ((cnt1 ne 0) && (cnt2 ne 0)) then begin
      WIDGET_CONTROL, wLeft, SET_VALUE=*unitArray[wh[0]]
      WIDGET_CONTROL, wRight, SET_VALUE=*unitArray[wh[0]]
      WIDGET_CONTROL, wLeft, SET_COMBOBOX_SELECT=ind1
      WIDGET_CONTROL, wRight, SET_COMBOBOX_SELECT=ind2
    endif
  endif
  
  state = {wType:wType, wLeft:wLeft, wLeftVal:wLeftVal, wRight:wRight, $
           wRightVal:wRightVal, wStatus:wStatus, unitArray:unitArray}
  pState = PTR_NEW(state, /NO_COPY)
  WIDGET_CONTROL, tlb, /REALIZE, SET_UVALUE=pState

  XMANAGER, 'idl_convert', tlb, /NO_BLOCK
  
end
