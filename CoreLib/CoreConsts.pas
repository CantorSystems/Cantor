(*
    The Unified Environment Core Library

    Core string messages and localization

    Copyright (c) 2008-2009 The Unified Environment Laboratory
*)

unit CoreConsts;

interface

const
  CP_LEGACY = 0; // CP_ACP or actually 7-bit for English

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
//  sIntfCastError      = 'Interface not supported';
//  sSafecallException  = 'Exception in safecall method';
  sOperationAborted   = 'Operation aborted';
  sOutOfMemory        = 'Out of memory';
  sInvalidPointer     = 'Invalid pointer operation';

  sReadAccess         = 'Read';
  sWriteAccess        = 'Write';

  sAccessViolation = 'Access violation at address %p. %hs of address %p';
  sModuleAccessViolation = 'Access violation at address %p in module "%s". %hs of address %p';

  sAbstractError    = 'Abstract method call'; // instead of meaningless "Abstract Error"
  sGeneralFault     = 'General fault %#02X';  // e. g. 0x0F for 15

{$IFOPT C+}
  sAssertError = '%hs (%hs, line %d)';
  sAssertionFailed = 'Assertion failed';
{$ENDIF}

  sSharingViolation = 'Sharing violation of object %hs';

implementation

end.

