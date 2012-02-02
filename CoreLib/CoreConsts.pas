(*
    The Unified Environment Core Library

    The Unified Environment Core Library

    Core library string messages

    Copyright (c) 2008-2012 The Unified Environment Laboratory

    Conditional defines:
      * Interfaces -- interface support
*)

unit CoreConsts;

interface

const
  CP_LEGACY = 1252; // we're using “”

  sDivByZero          = 'Division by zero';
  sRangeError         = 'Range check error';
  sIntOverflow        = 'Integer overflow';
  sInvalidOp          = 'Invalid floating point operation';
  sZeroDivide         = 'Floating point division by zero';
  sOverflow           = 'Floating point overflow';
  sUnderflow          = 'Floating point underflow';
  sInvalidCast        = 'Invalid class typecast';
  sPrivilege          = 'Privileged instruction';
  sControlC           = '^C';
  sStackOverflow      = 'Stack overflow';
{$IFDEF Interfaces}
  sIntfNotSupported   = 'Interface not supported';
  sSafecallException  = 'Exception in safecall method';
{$ENDIF}
  sOperationAborted   = 'Operation aborted';
  sOutOfMemory        = 'Out of memory';
  sInvalidPointer     = 'Invalid pointer operation';

  sReadAccess         = 'Read';
  sWriteAccess        = 'Write';

  sAccessViolation = 'Access violation at address %p. %hs of address %p';
  sModuleAccessViolation = 'Access violation at address %p in module “%ws”.'#10'%hs of address %p';

  sAbstractError    = 'Abstract method call'; // instead of meaningless "Abstract Error"
  sGeneralFault     = 'General fault %#02X';  // e. g. 0x0F for 15

{$IFOPT C+}
  sAssertError = '%hs (%hs, line %i)';
  sAssertionFailed = 'Assertion failed';
{$ENDIF}

  sConsistentRead = 'consistent read';
  sSyncUpdate = 'syncronized update';
  sExclusiveLock = 'exclusive lock';
  sSharingViolation = 'Sharing violation while trying to %s of %hs object';

  sIndexOutOfBounds = 'Index out of bounds %i..%i on %hs[%i]';

implementation

end.

