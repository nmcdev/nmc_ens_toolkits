# This is the IDL class that rules all
#
# Copyright (c) 2015-2015, Exelis Visual Information Solutions, Inc. All
# rights reserved. This software includes information which is
# proprietary to and a trade secret of Exelis Visual Information Solutions, Inc.
# It is not to be disclosed to anyone outside of this organization.
# Reproduction by any means whatsoever is prohibited without express
# written permission.
#
# Unit Tests:
#   To run the Python doctest unit tests, do:
#   >>> python -m doctest -v idlpy.py
#
import sys
import traceback

# Dynamically construct the library name from the Python major.minor version.
idllibrary = "pythonidl" + str(sys.version_info[0]) + str(sys.version_info[1])
pyidl = __import__(idllibrary)


class idl_stdout:
    def __init__(self):
        self.data = ''
    def write(self, s):
        self.data += s
    def flush(self):
    	self.data = ''


# subclass from type to create our own metaclass
class IDLType(type):
    """Metaclass for IDL"""
    def __init__(cls, name, bases, attrs):
        """Initialize the IDL interpreter."""
        super(IDLType, cls).__init__(name, bases, attrs)
        pyidl.initialize()

    @classmethod
    def __getattr__(cls, attr, extra=None):
        """
        Retrieve an IDL variable or call an IDL routine.
        
        For example, to retrieve a variable:
        >>> IDL.run("var = 'Hello, World!'")
        >>> var = IDL.var
        >>> print(var)
        Hello, World!

        Another example, calling an IDL routine:
        >>> result = IDL.findgen(10,20)
        >>> result.shape   # doctest: +ELLIPSIS
        (20..., 10...)
        """
        # In Python3 we get called with an extra argument from "help(IDL)"
        if (extra != None):
            return super(cls, attr).__getattr__(extra)
        # For internal Python attributes, just call our superclass
        if (attr[0] == '_' or attr == 'getdoc'):
            return getattr(super(IDLType, cls), attr)
        #*** print("class __getattr__: " + attr)
        # we need to ask IDL if this variable exists
        # if it doesn't, then we assume that it is an IDL function call.
        # unfortunately, this won't throw an error for mispelled variables...
        if (pyidl.hasVariable(attr)):
            return pyidl.getVariable(attr)
        return IDLRoutine(attr)

    @classmethod
    def __setattr__(cls, attr, value):
        """
        Set an IDL variable value.
        
        For example:
        >>> IDL.var = {'key1':5, 'key2': "value"}
        >>> IDL.run('PRINT, var, /IMPLIED')
        '{\\n    "key1": 5,\\n    "key2": "value"\\n}'
        """
        #*** print("class __setattr__")
		# add try/catch and fix up error msg
        pyidl.setVariable(attr, value)

        
class IDLRoutine(object):
    """Used to call an IDL function, procedure, or static method."""
    def __init__(self, fn):
        self.__dict__['fn'] = fn
    def __call__(self, *args, **kw):
        #*** print "%s%s%s" % (fn, args, kw)
        # To be able to call IDL procedures as functions, we need to
        # make sure that the IDL routine has been compiled.
        # Use /QUIET to ignore errors from non-existent or system routines.
        pyidl.callFunction("Resolve_Routine", (self.fn,),
            {"QUIET":1, "EITHER":1, "NO_RECOMPILE":0})
        return pyidl.callFunction(self.fn, args, kw)
    # If the user does IDL.MyThing.MySubthing, then assume that this is
    # a static method call. Example: IDL.Clipboard.Get()
    def __getattr__(self, attr):
        # For internal Python attributes, just call our superclass
        if (attr[0] == '_' or attr == 'getdoc'):
            return getattr(super(IDLRoutine, self), attr)
        # A generic wrapper that can call an IDL static method.
        def wrapper(*args, **kw):
            #*** print "%s%s%s" % (attr, args, kw)
            # Build up the name of the static method.
            staticMethod = self.fn + "." + attr
            # Add a fake HVID
            args = list(args)
            args.insert(0, -1)
            args = tuple(args)
            return pyidl.callMethod(staticMethod, args, kw)
        return wrapper

            
# Use a metaclass so we can have class methods (such as "run") and attributes
# that can only be invoked on the class, not an instance.
class IDL(IDLType("IDL", (object,), {})):
    """Used to invoke the run method, and get or set IDL $main$ variables.
    Also serves as the Python class for wrapping IDL objects."""
    
    # hvid is the IDL object heap identifier
    def __init__(self, hvid):
        """Create a Python object that wraps an IDL object with heap ID HVID."""
        if (hvid <= 0):
            raise ValueError("A valid IDL HVID must be passed in.")
        # traceback.print_stack()
        self.hvid = hvid
        # On construction, increment the IDL reference count
        pyidl.heapIncrRefCount(self.hvid)
        # Replace our class methods with ones that throws an error.
        def run(command, stdout=False):
            raise ValueError("IDL.run() must be called as a class method.")
        self.__dict__['run'] = run
        def exit():
            raise ValueError("IDL.exit() must be called as a class method.")
        self.__dict__['exit'] = exit

    def __del__(self):
        """On destruction of an IDL object, decrement the IDL reference count."""
        pyidl.heapDecrRefCount(self.hvid)


    def __repr__(self):
        """Create a representation of the IDL object using its properties."""
        strlist = IDL.string(self, implied=1)
        # If the result is a scalar string, just return it.
        if (strlist.__class__ == str): return strlist
        # Otherwise, if we have a list of strings, concat them together
        result = ''
        for idx, item in enumerate(strlist):
            result += item + '\n'
        return result

        
    def __str__(self):
        """Return a brief description of the IDL object."""
        idlClass = pyidl.callFunction("Obj_Class", (self,))
        format = "%s.%s <%i>  IDL Class: %s <HVID=%i>"
        return format % (self.__module__, self.__class__.__name__,
            id(self), idlClass, self.hvid)


    # Override the magic method for getattr for an IDL object
    # Note: This will not override getattr for the class
    def __getattr__(self, attr):
        """
        Retrieve a property value or call a method on the IDL object.
        
        For example:
        >>> p = IDL.plot(test=1, buffer=1)
        >>> p.color
        array([0, 0, 0], dtype=uint8)
        >>> result = p.copywindow()
        >>> result.shape   # doctest: +ELLIPSIS
        (512..., 640..., 3...)
        >>> p.close()
        """
        # For internal Python attributes, just call our superclass
        if (attr[0] == '_'):
            return getattr(super(IDL, self), attr)

        #*** print("instance __getattr__: " + attr)

        isMethod = pyidl.callFunction("Obj_HasMethod", (self, attr,))

        if (isMethod):
            # A generic wrapper that can call an IDL method.
            def wrapper(*args, **kw):
                #*** print "%s%s%s" % (attr, args, kw)
                args = list(args)
                args.insert(0, self.hvid)
                args = tuple(args)
                return pyidl.callMethod(attr, args, kw)
            return wrapper
        else:
            # otherwise retrieve the property from the IDL object
            args = (self.hvid, attr)
            result = pyidl.callMethod("GetPropByName", args)
            return result

            
    def __setattr__(self, attr, value):
        """
        Set a property value on the IDL object.
        
        For example:
        >>> p = IDL.plot(test=1, buffer=1)
        >>> p.color = 'red'
        >>> p.color
        array([255,   0,   0], dtype=uint8)
        """
        if (attr == "hvid"):
            self.__dict__['hvid'] = value
            return
        #*** print("instance __setattr__: " + attr)
        args = (self.hvid,)
        kwArgs = {}
        kwArgs.setdefault(attr, value)
        pyidl.callMethod("SetProperty", args, kwArgs)

        
    # We could have defined this in the IDLType above.
    # By defining it here the docs will show up with help(IDL).
    @classmethod
    def run(cls, command, stdout=False, silent=False):
        """
        Execute an IDL command.
        
        For example:
        >>> IDL.run('var = LINDGEN(100)')
        >>> IDL.run('print, TOTAL(var)')
        '      4950.00'
        """
        if (not stdout):
            prevStdout = sys.stdout
            idlstdout = idl_stdout()
            sys.stdout = idlstdout
        pyidl.execute(command, silent=silent)
        if (not stdout):
            sys.stdout = prevStdout
            result = idlstdout.data
            result = result.strip('\n')
            if (result == ''): return None
            return result
        return None

         
    # We could have defined this in the IDLType above.
    # By defining it here the docs will show up with help(IDL).
    @classmethod
    def exit(cls):
        """
        Exit the IDL process.
        """
        pyidl.cleanup()
