unit uPSI_rcx_constants;
{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis. 
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface
 
uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;
 
type 
(*----------------------------------------------------------------------------*)
  TPSImport_rcx_constants = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_rcx_constants(CL: TPSPascalCompiler);

{ run-time registration functions }

procedure Register;

implementation


uses
   rcx_constants
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_rcx_constants]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_rcx_constants(CL: TPSPascalCompiler);
begin
 CL.AddConstantN('kRCX_PingOp','LongWord').SetUInt( $10);
 CL.AddConstantN('kRCX_MemMapOp','LongWord').SetUInt( $20);
 CL.AddConstantN('kRCX_BatteryLevelOp','LongWord').SetUInt( $30);
 CL.AddConstantN('kRCX_DeleteTasksOp','LongWord').SetUInt( $40);
 CL.AddConstantN('kRCX_StopAllOp','LongWord').SetUInt( $50);
 CL.AddConstantN('kRCX_PBTurnOffOp','LongWord').SetUInt( $60);
 CL.AddConstantN('kRCX_DeleteSubsOp','LongWord').SetUInt( $70);
 CL.AddConstantN('kRCX_ClearSound','LongWord').SetUInt( $80);
 CL.AddConstantN('kRCX_ClearMsgOp','LongWord').SetUInt( $90);
 CL.AddConstantN('kRCX_ExitAccessCtrlOp','LongWord').SetUInt( $a0);
 CL.AddConstantN('kRCX_ExitEventChkOp','LongWord').SetUInt( $b0);
 CL.AddConstantN('kRCX_LSCalibrateOp','LongWord').SetUInt( $c0);
 CL.AddConstantN('kRCX_MuteSoundOp','LongWord').SetUInt( $d0);
 CL.AddConstantN('kRCX_UnmuteSoundOp','LongWord').SetUInt( $e0);
 CL.AddConstantN('kRCX_PopStackEntryOp','LongWord').SetUInt( $01);
 CL.AddConstantN('kRCX_UploadEepromOp','LongWord').SetUInt( $11);
 CL.AddConstantN('kRCX_ClearTachoOp','LongWord').SetUInt( $11);
 CL.AddConstantN('kRCX_OutputModeOp','LongWord').SetUInt( $21);
 CL.AddConstantN('kRCX_IRModeOp','LongWord').SetUInt( $31);
 CL.AddConstantN('kRCX_DriveOp','LongWord').SetUInt( $41);
 CL.AddConstantN('kRCX_PlaySoundOp','LongWord').SetUInt( $51);
 CL.AddConstantN('kRCX_DeleteTaskOp','LongWord').SetUInt( $61);
 CL.AddConstantN('kRCX_StartTaskOp','LongWord').SetUInt( $71);
 CL.AddConstantN('kRCX_StopTaskOp','LongWord').SetUInt( $81);
 CL.AddConstantN('kRCX_SelectProgramOp','LongWord').SetUInt( $91);
 CL.AddConstantN('kRCX_ClearTimerOp','LongWord').SetUInt( $a1);
 CL.AddConstantN('kRCX_AutoOffOp','LongWord').SetUInt( $b1);
 CL.AddConstantN('kRCX_DeleteSubOp','LongWord').SetUInt( $c1);
 CL.AddConstantN('kRCX_ClearSensorOp','LongWord').SetUInt( $d1);
 CL.AddConstantN('kRCX_OutputDirOp','LongWord').SetUInt( $e1);
 CL.AddConstantN('kRCX_PlayToneVarOp','LongWord').SetUInt( $02);
 CL.AddConstantN('kRCX_PollOp','LongWord').SetUInt( $12);
 CL.AddConstantN('kRCX_SetWatchOp','LongWord').SetUInt( $22);
 CL.AddConstantN('kRCX_InputTypeOp','LongWord').SetUInt( $32);
 CL.AddConstantN('kRCX_InputModeOp','LongWord').SetUInt( $42);
 CL.AddConstantN('kRCX_SetDatalogOp','LongWord').SetUInt( $52);
 CL.AddConstantN('kRCX_DatalogOp','LongWord').SetUInt( $62);
 CL.AddConstantN('kRCX_JumpOp','LongWord').SetUInt( $72);
 CL.AddConstantN('kRCX_SetLoopOp','LongWord').SetUInt( $82);
 CL.AddConstantN('kRCX_CheckLoopOp','LongWord').SetUInt( $92);
 CL.AddConstantN('kRCX_SendMessageOp','LongWord').SetUInt( $b2);
 CL.AddConstantN('kRCX_SendUARTDataOp','LongWord').SetUInt( $c2);
 CL.AddConstantN('kRCX_OnWaitOp','LongWord').SetUInt( $c2);
 CL.AddConstantN('kRCX_Remote','LongWord').SetUInt( $d2);
 CL.AddConstantN('kRCX_VLLOp','LongWord').SetUInt( $e2);
 CL.AddConstantN('kRCX_DecVarJmpLTZOp','LongWord').SetUInt( $f2);
 CL.AddConstantN('kRCX_DirectEventOp','LongWord').SetUInt( $03);
 CL.AddConstantN('kRCX_OutputPowerOp','LongWord').SetUInt( $13);
 CL.AddConstantN('kRCX_PlayToneOp','LongWord').SetUInt( $23);
 CL.AddConstantN('kRCX_DisplayOp','LongWord').SetUInt( $33);
 CL.AddConstantN('kRCX_WaitOp','LongWord').SetUInt( $43);
 CL.AddConstantN('kRCX_OnWaitDifferentOp','LongWord').SetUInt( $53);
 CL.AddConstantN('kRCX_PollMemoryOp','LongWord').SetUInt( $63);
 CL.AddConstantN('kRCX_EnterAccessCtrlOp','LongWord').SetUInt( $73);
 CL.AddConstantN('kRCX_SetFeedbackOp','LongWord').SetUInt( $83);
 CL.AddConstantN('kRCX_SetEventOp','LongWord').SetUInt( $93);
 CL.AddConstantN('kRCX_GOutputPowerOp','LongWord').SetUInt( $a3);
 CL.AddConstantN('kRCX_LSUpperThreshOp','LongWord').SetUInt( $b3);
 CL.AddConstantN('kRCX_LSLowerThreshOp','LongWord').SetUInt( $c3);
 CL.AddConstantN('kRCX_LSHysteresisOp','LongWord').SetUInt( $d3);
 CL.AddConstantN('kRCX_PushStackEntryOp','LongWord').SetUInt( $e3);
 CL.AddConstantN('kRCX_LSBlinkTimeOp','LongWord').SetUInt( $e3);
 CL.AddConstantN('kRCX_LDecVarJmpLTZOp','LongWord').SetUInt( $f3);
 CL.AddConstantN('kRCX_CalibrateEventOp','LongWord').SetUInt( $04);
 CL.AddConstantN('kRCX_SetVarOp','LongWord').SetUInt( $14);
 CL.AddConstantN('kRCX_SumVarOp','LongWord').SetUInt( $24);
 CL.AddConstantN('kRCX_SubVarOp','LongWord').SetUInt( $34);
 CL.AddConstantN('kRCX_DivVarOp','LongWord').SetUInt( $44);
 CL.AddConstantN('kRCX_MulVarOp','LongWord').SetUInt( $54);
 CL.AddConstantN('kRCX_SgnVarOp','LongWord').SetUInt( $64);
 CL.AddConstantN('kRCX_AbsVarOp','LongWord').SetUInt( $74);
 CL.AddConstantN('kRCX_AndVarOp','LongWord').SetUInt( $84);
 CL.AddConstantN('kRCX_OrVarOp','LongWord').SetUInt( $94);
 CL.AddConstantN('kRCX_UploadDatalogOp','LongWord').SetUInt( $a4);
 CL.AddConstantN('kRCX_SEnterEventChkOp','LongWord').SetUInt( $b4);
 CL.AddConstantN('kRCX_SetTimerLimitOp','LongWord').SetUInt( $c4);
 CL.AddConstantN('kRCX_SetCounterOp','LongWord').SetUInt( $d4);
 CL.AddConstantN('kRCX_SetSourceValueOp','LongWord').SetUInt( $05);
 CL.AddConstantN('kRCX_UnlockOp','LongWord').SetUInt( $15);
 CL.AddConstantN('kRCX_BeginTaskOp','LongWord').SetUInt( $25);
 CL.AddConstantN('kRCX_BeginSubOp','LongWord').SetUInt( $35);
 CL.AddConstantN('kRCX_DownloadOp','LongWord').SetUInt( $45);
 CL.AddConstantN('kRCX_BootModeOp','LongWord').SetUInt( $65);
 CL.AddConstantN('kRCX_BeginFirmwareOp','LongWord').SetUInt( $75);
 CL.AddConstantN('kRCX_SCheckDoOp','LongWord').SetUInt( $85);
 CL.AddConstantN('kRCX_LCheckDoOp','LongWord').SetUInt( $95);
 CL.AddConstantN('kRCX_UnlockFirmOp','LongWord').SetUInt( $a5);
 CL.AddConstantN('kRCX_LEnterEventChkOp','LongWord').SetUInt( $b5);
 CL.AddConstantN('kRCX_FindOp','LongWord').SetUInt( $d5);
 CL.AddConstantN('kRCX_ScoutRulesOp','LongWord').SetUInt( $d5);
 CL.AddConstantN('kRCX_ViewSourceValOp','LongWord').SetUInt( $e5);
 CL.AddConstantN('kRCX_ClearAllEventsOp','LongWord').SetUInt( $06);
 CL.AddConstantN('kRCX_ClearRelTableOp','LongWord').SetUInt( $36);
 CL.AddConstantN('kRCX_EndOfSubOp','LongWord').SetUInt( $F6);
 CL.AddConstantN('kRCX_GoSubOp','LongWord').SetUInt( $17);
 CL.AddConstantN('kRCX_SJumpOp','LongWord').SetUInt( $27);
 CL.AddConstantN('kRCX_SChkLoopCtrOp','LongWord').SetUInt( $37);
 CL.AddConstantN('kRCX_ScoutOp','LongWord').SetUInt( $47);
 CL.AddConstantN('kRCX_SoundOp','LongWord').SetUInt( $57);
 CL.AddConstantN('kRCX_GOutputModeOp','LongWord').SetUInt( $67);
 CL.AddConstantN('kRCX_GOutputDirOp','LongWord').SetUInt( $77);
 CL.AddConstantN('kRCX_LightOp','LongWord').SetUInt( $87);
 CL.AddConstantN('kRCX_IncCounterOp','LongWord').SetUInt( $97);
 CL.AddConstantN('kRCX_DecCounterOp','LongWord').SetUInt( $a7);
 CL.AddConstantN('kRCX_ClearCounterOp','LongWord').SetUInt( $b7);
 CL.AddConstantN('kRCX_PlaySysMoodOp','LongWord').SetUInt( $c7);
 CL.AddConstantN('kRCX_SetPriorityOp','LongWord').SetUInt( $d7);
 CL.AddConstantN('kRCX_PlaySysSndVarOp','LongWord').SetUInt( $e7);
 CL.AddConstantN('kRCX_Message','LongWord').SetUInt( $f7);
 CL.AddConstantN('kRCX_OutputFloat','LongInt').SetInt( 0);
 CL.AddConstantN('kRCX_OutputOff','LongWord').SetUInt( $40);
 CL.AddConstantN('kRCX_OutputOn','LongWord').SetUInt( $80);
 CL.AddConstantN('kRCX_OutputBackward','LongInt').SetInt( 0);
 CL.AddConstantN('kRCX_OutputToggle','LongWord').SetUInt( $40);
 CL.AddConstantN('kRCX_OutputForward','LongWord').SetUInt( $80);
 CL.AddConstantN('kNXT_VMState_Idle','LongInt').SetInt( 0);
 CL.AddConstantN('kNXT_VMState_RunFree','LongInt').SetInt( 1);
 CL.AddConstantN('kNXT_VMState_Single','LongInt').SetInt( 2);
 CL.AddConstantN('kNXT_VMState_Pause','LongInt').SetInt( 3);
 CL.AddConstantN('kNXT_VMState_Reset','LongInt').SetInt( 4);
 CL.AddConstantN('kNXT_NoResponseMask','LongWord').SetUInt( $80);
 CL.AddConstantN('kNXT_DirectCmd','LongWord').SetUInt( $00);
 CL.AddConstantN('kNXT_SystemCmd','LongWord').SetUInt( $01);
 CL.AddConstantN('kNXT_CmdReply','LongWord').SetUInt( $02);
 CL.AddConstantN('kNXT_DirectCmdNoReply','LongWord').SetUInt( $80);
 CL.AddConstantN('kNXT_SystemCmdNoReply','LongWord').SetUInt( $81);
 CL.AddConstantN('kNXT_MaxBytes','LongInt').SetInt( 64);
 CL.AddConstantN('kNXT_NameMaxLen','LongInt').SetInt( 15);
 CL.AddConstantN('kNXT_DCStartProgram','LongWord').SetUInt( $00);
 CL.AddConstantN('kNXT_DCStopProgram','LongWord').SetUInt( $01);
 CL.AddConstantN('kNXT_DCPlaySoundFile','LongWord').SetUInt( $02);
 CL.AddConstantN('kNXT_DCPlayTone','LongWord').SetUInt( $03);
 CL.AddConstantN('kNXT_DCSetOutputState','LongWord').SetUInt( $04);
 CL.AddConstantN('kNXT_DCSetInputMode','LongWord').SetUInt( $05);
 CL.AddConstantN('kNXT_DCGetOutputState','LongWord').SetUInt( $06);
 CL.AddConstantN('kNXT_DCGetInputValues','LongWord').SetUInt( $07);
 CL.AddConstantN('kNXT_DCResetInputScaledValue','LongWord').SetUInt( $08);
 CL.AddConstantN('kNXT_DCMessageWrite','LongWord').SetUInt( $09);
 CL.AddConstantN('kNXT_DCResetMotorPosition','LongWord').SetUInt( $0A);
 CL.AddConstantN('kNXT_DCGetBatteryLevel','LongWord').SetUInt( $0B);
 CL.AddConstantN('kNXT_DCStopSoundPlayback','LongWord').SetUInt( $0C);
 CL.AddConstantN('kNXT_DCKeepAlive','LongWord').SetUInt( $0D);
 CL.AddConstantN('kNXT_DCLSGetStatus','LongWord').SetUInt( $0E);
 CL.AddConstantN('kNXT_DCLSWrite','LongWord').SetUInt( $0F);
 CL.AddConstantN('kNXT_DCLSRead','LongWord').SetUInt( $10);
 CL.AddConstantN('kNXT_DCGetCurrentProgramName','LongWord').SetUInt( $11);
 CL.AddConstantN('kNXT_DCGetButtonState','LongWord').SetUInt( $12);
 CL.AddConstantN('kNXT_DCMessageRead','LongWord').SetUInt( $13);
 CL.AddConstantN('kNXT_DCReserved1','LongWord').SetUInt( $14);
 CL.AddConstantN('kNXT_DCReserved2','LongWord').SetUInt( $15);
 CL.AddConstantN('kNXT_DCReserved3','LongWord').SetUInt( $16);
 CL.AddConstantN('kNXT_DCReserved4','LongWord').SetUInt( $17);
 CL.AddConstantN('kNXT_DCReserved5','LongWord').SetUInt( $18);
 CL.AddConstantN('kNXT_DCDatalogRead','LongWord').SetUInt( $19);
 CL.AddConstantN('kNXT_DCDatalogSetTimes','LongWord').SetUInt( $1a);
 CL.AddConstantN('kNXT_DCBTGetContactCount','LongWord').SetUInt( $1b);
 CL.AddConstantN('kNXT_DCBTGetContactName','LongWord').SetUInt( $1c);
 CL.AddConstantN('kNXT_DCBTGetConnectionCount','LongWord').SetUInt( $1d);
 CL.AddConstantN('kNXT_DCBTGetConnectionName','LongWord').SetUInt( $1e);
 CL.AddConstantN('kNXT_DCSetProperty','LongWord').SetUInt( $1f);
 CL.AddConstantN('kNXT_DCGetProperty','LongWord').SetUInt( $20);
 CL.AddConstantN('kNXT_DCUpdateResetCount','LongWord').SetUInt( $21);
 CL.AddConstantN('kNXT_DCSetVMState','LongWord').SetUInt( $22);
 CL.AddConstantN('kNXT_DCGetVMState','LongWord').SetUInt( $23);
 CL.AddConstantN('kNXT_DCSetBreakpoints','LongWord').SetUInt( $24);
 CL.AddConstantN('kNXT_DCGetBreakpoints','LongWord').SetUInt( $25);
 CL.AddConstantN('kNXT_Property_BTOnOff','LongWord').SetUInt( $0);
 CL.AddConstantN('kNXT_Property_SoundLevel','LongWord').SetUInt( $1);
 CL.AddConstantN('kNXT_Property_SleepTimeout','LongWord').SetUInt( $2);
 CL.AddConstantN('kNXT_Property_Undefined1','LongWord').SetUInt( $3);
 CL.AddConstantN('kNXT_Property_Undefined2','LongWord').SetUInt( $4);
 CL.AddConstantN('kNXT_Property_Undefined3','LongWord').SetUInt( $5);
 CL.AddConstantN('kNXT_Property_Undefined4','LongWord').SetUInt( $6);
 CL.AddConstantN('kNXT_Property_Undefined5','LongWord').SetUInt( $7);
 CL.AddConstantN('kNXT_Property_Undefined6','LongWord').SetUInt( $8);
 CL.AddConstantN('kNXT_Property_Undefined7','LongWord').SetUInt( $9);
 CL.AddConstantN('kNXT_Property_Undefined8','LongWord').SetUInt( $a);
 CL.AddConstantN('kNXT_Property_Undefined9','LongWord').SetUInt( $b);
 CL.AddConstantN('kNXT_Property_Undefined10','LongWord').SetUInt( $c);
 CL.AddConstantN('kNXT_Property_Undefined11','LongWord').SetUInt( $d);
 CL.AddConstantN('kNXT_Property_Undefined12','LongWord').SetUInt( $e);
 CL.AddConstantN('kNXT_Property_Debugging','LongWord').SetUInt( $f);
 CL.AddConstantN('kNXT_SCOpenRead','LongWord').SetUInt( $80);
 CL.AddConstantN('kNXT_SCOpenWrite','LongWord').SetUInt( $81);
 CL.AddConstantN('kNXT_SCRead','LongWord').SetUInt( $82);
 CL.AddConstantN('kNXT_SCWrite','LongWord').SetUInt( $83);
 CL.AddConstantN('kNXT_SCClose','LongWord').SetUInt( $84);
 CL.AddConstantN('kNXT_SCDelete','LongWord').SetUInt( $85);
 CL.AddConstantN('kNXT_SCFindFirst','LongWord').SetUInt( $86);
 CL.AddConstantN('kNXT_SCFindNext','LongWord').SetUInt( $87);
 CL.AddConstantN('kNXT_SCGetVersions','LongWord').SetUInt( $88);
 CL.AddConstantN('kNXT_SCOpenWriteLinear','LongWord').SetUInt( $89);
 CL.AddConstantN('kNXT_SCOpenReadLinear','LongWord').SetUInt( $8A);
 CL.AddConstantN('kNXT_SCOpenWriteData','LongWord').SetUInt( $8B);
 CL.AddConstantN('kNXT_SCOpenAppendData','LongWord').SetUInt( $8C);
 CL.AddConstantN('kNXT_SCUnknown1','LongWord').SetUInt( $8D);
 CL.AddConstantN('kNXT_SCUnknown2','LongWord').SetUInt( $8E);
 CL.AddConstantN('kNXT_SCUnknown3','LongWord').SetUInt( $8F);
 CL.AddConstantN('kNXT_SCFindFirstModule','LongWord').SetUInt( $90);
 CL.AddConstantN('kNXT_SCFindNextModule','LongWord').SetUInt( $91);
 CL.AddConstantN('kNXT_SCCloseModuleHandle','LongWord').SetUInt( $92);
 CL.AddConstantN('kNXT_SCUnknown4','LongWord').SetUInt( $93);
 CL.AddConstantN('kNXT_SCIOMapRead','LongWord').SetUInt( $94);
 CL.AddConstantN('kNXT_SCIOMapWrite','LongWord').SetUInt( $95);
 CL.AddConstantN('kNXT_SCUnknown5','LongWord').SetUInt( $96);
 CL.AddConstantN('kNXT_SCBootCommand','LongWord').SetUInt( $97);
 CL.AddConstantN('kNXT_SCSetBrickName','LongWord').SetUInt( $98);
 CL.AddConstantN('kNXT_SCUnknown6','LongWord').SetUInt( $99);
 CL.AddConstantN('kNXT_SCGetBTAddress','LongWord').SetUInt( $9A);
 CL.AddConstantN('kNXT_SCGetDeviceInfo','LongWord').SetUInt( $9B);
 CL.AddConstantN('kNXT_SCUnknown7','LongWord').SetUInt( $9C);
 CL.AddConstantN('kNXT_SCUnknown8','LongWord').SetUInt( $9D);
 CL.AddConstantN('kNXT_SCUnknown9','LongWord').SetUInt( $9E);
 CL.AddConstantN('kNXT_SCUnknown10','LongWord').SetUInt( $9F);
 CL.AddConstantN('kNXT_SCDeleteUserFlash','LongWord').SetUInt( $A0);
 CL.AddConstantN('kNXT_SCPollCommandLen','LongWord').SetUInt( $A1);
 CL.AddConstantN('kNXT_SCPollCommand','LongWord').SetUInt( $A2);
 CL.AddConstantN('kNXT_SCRenameFile','LongWord').SetUInt( $A3);
 CL.AddConstantN('kNXT_SCBTFactoryReset','LongWord').SetUInt( $A4);
 CL.AddConstantN('kNXT_StatusSuccess','LongWord').SetUInt( $00);
 CL.AddConstantN('kNXT_StatusNoMoreHandles','LongWord').SetUInt( $81);
 CL.AddConstantN('kNXT_StatusNoSpace','LongWord').SetUInt( $82);
 CL.AddConstantN('kNXT_StatusNoMoreFiles','LongWord').SetUInt( $83);
 CL.AddConstantN('kNXT_StatusEOFExpected','LongWord').SetUInt( $84);
 CL.AddConstantN('kNXT_StatusEOF','LongWord').SetUInt( $85);
 CL.AddConstantN('kNXT_StatusNotLinearFile','LongWord').SetUInt( $86);
 CL.AddConstantN('kNXT_StatusFileNotFound','LongWord').SetUInt( $87);
 CL.AddConstantN('kNXT_StatusHandleClosed','LongWord').SetUInt( $88);
 CL.AddConstantN('kNXT_StatusNoLinearSpace','LongWord').SetUInt( $89);
 CL.AddConstantN('kNXT_StatusUndefinedErr','LongWord').SetUInt( $8a);
 CL.AddConstantN('kNXT_StatusFileIsBusy','LongWord').SetUInt( $8b);
 CL.AddConstantN('kNXT_StatusNoWriteBufs','LongWord').SetUInt( $8c);
 CL.AddConstantN('kNXT_StatusAppendNotPoss','LongWord').SetUInt( $8d);
 CL.AddConstantN('kNXT_StatusFileIsFull','LongWord').SetUInt( $8e);
 CL.AddConstantN('kNXT_StatusFileExists','LongWord').SetUInt( $8f);
 CL.AddConstantN('kNXT_StatusModuleNotFound','LongWord').SetUInt( $90);
 CL.AddConstantN('kNXT_StatusOutOfBoundary','LongWord').SetUInt( $91);
 CL.AddConstantN('kNXT_StatusIllegalFilname','LongWord').SetUInt( $92);
 CL.AddConstantN('kNXT_StatusIllegalHandle','LongWord').SetUInt( $93);
 CL.AddConstantN('NXT_MODULE_COUNT','LongInt').SetInt( 12);
 CL.AddConstantN('kNXT_TT_Cmd','LongWord').SetUInt( $01);
 CL.AddConstantN('kNXT_TT_Output','LongWord').SetUInt( $02);
 CL.AddConstantN('kNXT_TT_Input','LongWord').SetUInt( $03);
 CL.AddConstantN('kNXT_TT_Button','LongWord').SetUInt( $04);
 CL.AddConstantN('kNXT_TT_Comm','LongWord').SetUInt( $05);
 CL.AddConstantN('kNXT_TT_IOCtrl','LongWord').SetUInt( $06);
 CL.AddConstantN('kNXT_TT_Led','LongWord').SetUInt( $07);
 CL.AddConstantN('kNXT_TT_Sound','LongWord').SetUInt( $08);
 CL.AddConstantN('kNXT_TT_Loader','LongWord').SetUInt( $09);
 CL.AddConstantN('kNXT_TT_Display','LongWord').SetUInt( $0A);
 CL.AddConstantN('kNXT_TT_LowSpeed','LongWord').SetUInt( $0B);
 CL.AddConstantN('kNXT_TT_UI','LongWord').SetUInt( $0C);
 CL.AddConstantN('kNXT_PidLEGOGroup','LongWord').SetUInt( $01);
 CL.AddConstantN('kNXT_ModuleCmd','LongWord').SetUInt( $00010001);
 CL.AddConstantN('kNXT_ModuleOutput','LongWord').SetUInt( $00020001);
 CL.AddConstantN('kNXT_ModuleInput','LongWord').SetUInt( $00030001);
 CL.AddConstantN('kNXT_ModuleButton','LongWord').SetUInt( $00040001);
 CL.AddConstantN('kNXT_ModuleComm','LongWord').SetUInt( $00050001);
 CL.AddConstantN('kNXT_ModuleIOCtrl','LongWord').SetUInt( $00060001);
 CL.AddConstantN('kNXT_ModuleLed','LongWord').SetUInt( $00070001);
 CL.AddConstantN('kNXT_ModuleSound','LongWord').SetUInt( $00080001);
 CL.AddConstantN('kNXT_ModuleLoader','LongWord').SetUInt( $00090001);
 CL.AddConstantN('kNXT_ModuleDisplay','LongWord').SetUInt( $000A0001);
 CL.AddConstantN('kNXT_ModuleLowSpeed','LongWord').SetUInt( $000B0001);
 CL.AddConstantN('kNXT_ModuleUI','LongWord').SetUInt( $000C0001);
 CL.AddConstantN('kNXT_ModuleCmdName','String').SetString( 'Command.mod');
 CL.AddConstantN('kNXT_ModuleOutputName','String').SetString( 'Output.mod');
 CL.AddConstantN('kNXT_ModuleInputName','String').SetString( 'Input.mod');
 CL.AddConstantN('kNXT_ModuleButtonName','String').SetString( 'Button.mod');
 CL.AddConstantN('kNXT_ModuleCommName','String').SetString( 'Comm.mod');
 CL.AddConstantN('kNXT_ModuleIOCtrlName','String').SetString( 'IOCtrl.mod');
 CL.AddConstantN('kNXT_ModuleLedName','String').SetString( 'Led.mod');
 CL.AddConstantN('kNXT_ModuleSoundName','String').SetString( 'Sound.mod');
 CL.AddConstantN('kNXT_ModuleLoaderName','String').SetString( 'Loader.mod');
 CL.AddConstantN('kNXT_ModuleDisplayName','String').SetString( 'Display.mod');
 CL.AddConstantN('kNXT_ModuleLowSpeedName','String').SetString( 'Low Speed.mod');
 CL.AddConstantN('kNXT_ModuleUIName','String').SetString( 'Ui.mod');
  CL.AddTypeS('TNXTModule', 'record ID : Cardinal; Name : String; end');
  CL.AddTypeS('TRcxValueType', 'Integer');
 CL.AddConstantN('kRCX_VariableType','LongInt').SetInt( 0);
 CL.AddConstantN('kRCX_TimerType','LongInt').SetInt( 1);
 CL.AddConstantN('kRCX_ConstantType','LongInt').SetInt( 2);
 CL.AddConstantN('kRCX_OutputStatusType','LongInt').SetInt( 3);
 CL.AddConstantN('kRCX_RandomType','LongInt').SetInt( 4);
 CL.AddConstantN('kRCX_TachCounterType','LongInt').SetInt( 5);
 CL.AddConstantN('kRCX_TachSpeedType','LongInt').SetInt( 6);
 CL.AddConstantN('kRCX_OutputCurrentType','LongInt').SetInt( 7);
 CL.AddConstantN('kRCX_MotorPowerSignedType','LongInt').SetInt( 5);
 CL.AddConstantN('kRCX_IntrinsicIndGlobalType','LongInt').SetInt( 6);
 CL.AddConstantN('kRCX_MotorBrakePowerType','LongInt').SetInt( 7);
 CL.AddConstantN('kRCX_ProgramSlotType','LongInt').SetInt( 8);
 CL.AddConstantN('kRCX_InputValueType','LongInt').SetInt( 9);
 CL.AddConstantN('kRCX_InputTypeType','LongInt').SetInt( 10);
 CL.AddConstantN('kRCX_InputModeType','LongInt').SetInt( 11);
 CL.AddConstantN('kRCX_InputRawType','LongInt').SetInt( 12);
 CL.AddConstantN('kRCX_InputBooleanType','LongInt').SetInt( 13);
 CL.AddConstantN('kRCX_WatchType','LongInt').SetInt( 14);
 CL.AddConstantN('kRCX_MessageType','LongInt').SetInt( 15);
 CL.AddConstantN('kRCX_AGCType','LongInt').SetInt( 16);
 CL.AddConstantN('kRCX_MotorPower128Type','LongInt').SetInt( 16);
 CL.AddConstantN('kRCX_GlobalMotorStatusType','LongInt').SetInt( 17);
 CL.AddConstantN('kRCX_ScoutRulesType','LongInt').SetInt( 18);
 CL.AddConstantN('kRCX_ScoutLightParamsType','LongInt').SetInt( 19);
 CL.AddConstantN('kRCX_ScoutTimerLimitType','LongInt').SetInt( 20);
 CL.AddConstantN('kRCX_SpybotStackType','LongInt').SetInt( 18);
 CL.AddConstantN('kRCX_SpybotTimerCtrlType','LongInt').SetInt( 19);
 CL.AddConstantN('kRCX_SpybotEepromType','LongInt').SetInt( 20);
 CL.AddConstantN('kRCX_EventTypeType','LongInt').SetInt( 18);
 CL.AddConstantN('kRCX_EventType','LongInt').SetInt( 19);
 CL.AddConstantN('kRCX_EventCountsType','LongInt').SetInt( 20);
 CL.AddConstantN('kRCX_CounterType','LongInt').SetInt( 21);
 CL.AddConstantN('kRCX_ScoutCounterLimitType','LongInt').SetInt( 22);
 CL.AddConstantN('kRCX_SpybotLEDType','LongInt').SetInt( 22);
 CL.AddConstantN('kRCX_1MSTimerType','LongInt').SetInt( 22);
 CL.AddConstantN('kRCX_TaskEventsType','LongInt').SetInt( 23);
 CL.AddConstantN('kRCX_ScoutEventFBType','LongInt').SetInt( 24);
 CL.AddConstantN('kRCX_SystemType','LongInt').SetInt( 24);
 CL.AddConstantN('kRCX_EventStateType','LongInt').SetInt( 25);
 CL.AddConstantN('kRCX_TenMSTimerType','LongInt').SetInt( 26);
 CL.AddConstantN('kRCX_ClickCounterType','LongInt').SetInt( 27);
 CL.AddConstantN('kRCX_UpperThresholdType','LongInt').SetInt( 28);
 CL.AddConstantN('kRCX_LowerThresholdType','LongInt').SetInt( 29);
 CL.AddConstantN('kRCX_HysteresisType','LongInt').SetInt( 30);
 CL.AddConstantN('kRCX_DurationType','LongInt').SetInt( 31);
 CL.AddConstantN('kRCX_SpybotTaskIDType','LongInt').SetInt( 32);
 CL.AddConstantN('kRCX_MotorPower8Type','LongInt').SetInt( 32);
 CL.AddConstantN('kRCX_UARTSetupType','LongInt').SetInt( 33);
 CL.AddConstantN('kRCX_BatteryLevelType','LongInt').SetInt( 34);
 CL.AddConstantN('kRCX_FirmwareVersionType','LongInt').SetInt( 35);
 CL.AddConstantN('kRCX_IndirectVarType','LongInt').SetInt( 36);
 CL.AddConstantN('kRCX_DatalogTypeIndirectType','LongInt').SetInt( 37);
 CL.AddConstantN('kRCX_DatalogTypeDirectType','LongInt').SetInt( 38);
 CL.AddConstantN('kRCX_DatalogValueIndirectType','LongInt').SetInt( 39);
 CL.AddConstantN('kRCX_DatalogValueDirectType','LongInt').SetInt( 40);
 CL.AddConstantN('kRCX_DatalogRawIndirectType','LongInt').SetInt( 41);
 CL.AddConstantN('kRCX_DatalogRawDirectType','LongInt').SetInt( 42);
 CL.AddConstantN('kRCX_SpybotGameNotesType','LongInt').SetInt( 43);
 CL.AddConstantN('kRCX_SpybotRobotDistType','LongInt').SetInt( 45);
 CL.AddConstantN('kRCX_SpybotRobotDirType','LongInt').SetInt( 46);
 CL.AddConstantN('kRCX_SpybotRobotOrientType','LongInt').SetInt( 47);
 CL.AddConstantN('kRCX_SpybotRobotIDType','LongInt').SetInt( 49);
 CL.AddConstantN('kRCX_SpybotRobotTargetType','LongInt').SetInt( 50);
 CL.AddConstantN('kRCX_SpybotPingCtrlType','LongInt').SetInt( 51);
 CL.AddConstantN('kRCX_SpybotBeaconCtrlType','LongInt').SetInt( 52);
 CL.AddConstantN('kRCX_SpybotSoundCtrlType','LongInt').SetInt( 53);
 CL.AddConstantN('kRCX_SpybotIndEepromType','LongInt').SetInt( 54);
 CL.AddConstantN('kRCX_GlobalVarType','LongInt').SetInt( 43);
 CL.AddConstantN('kRCX_IndirectGlobalIntType','LongInt').SetInt( 44);
 CL.AddConstantN('kRCX_IndexedGlobalConstType','LongInt').SetInt( 47);
 CL.AddConstantN('kRCX_StackVarType','LongInt').SetInt( 49);
 CL.AddConstantN('kRCX_ConstantVarType','LongInt').SetInt( 50);
 CL.AddConstantN('kRCX_FunctionRetValWordType','LongInt').SetInt( 51);
 CL.AddConstantN('kRCX_VarByteType','LongInt').SetInt( 54);
 CL.AddConstantN('kRCX_VarWordType','LongInt').SetInt( 55);
 CL.AddConstantN('kRCX_TaskStackVarByteType','LongInt').SetInt( 57);
 CL.AddConstantN('kRCX_TaskStackVarWordType','LongInt').SetInt( 58);
 CL.AddConstantN('kRCX_TaskVarType','LongInt').SetInt( 60);
 CL.AddConstantN('kRCX_TaskStackAddressType','LongInt').SetInt( 61);
 CL.AddConstantN('kRCX_TaskStackSizeType','LongInt').SetInt( 62);
 CL.AddConstantN('kRCX_InputRaw','LongInt').SetInt( 0);
 CL.AddConstantN('kRCX_InputBoolean','LongWord').SetUInt( $20);
 CL.AddConstantN('kRCX_InputEdgeCounter','LongWord').SetUInt( $40);
 CL.AddConstantN('kRCX_InputPeriodicCounter','LongWord').SetUInt( $60);
 CL.AddConstantN('kRCX_InputPercentage','LongWord').SetUInt( $80);
 CL.AddConstantN('kRCX_InputCelcius','LongWord').SetUInt( $a0);
 CL.AddConstantN('kRCX_InputFahrenheit','LongWord').SetUInt( $c0);
 CL.AddConstantN('kRCX_InputAngle','LongWord').SetUInt( $e0);
  CL.AddTypeS('TRcxInputType', '( kRCX_InputNone, kRCX_InputSwitch, kRCX_InputT'
   +'emp, kRCX_InputLight, kRCX_InputRotation, kRCX_InputID0, kRCX_InputID1, kR'
   +'CX_InputID2 )');
  CL.AddTypeS('TRcxRelation', '( kRCX_LessOrEqual, kRCX_GreaterOrEqual, kRCX_No'
   +'tEqualTo, kRCX_EqualTo )');
 CL.AddConstantN('kRCX_OK','LongInt').SetInt( 0);
 CL.AddConstantN('kRCX_OpenSerialError','LongInt').SetInt( - 1);
 CL.AddConstantN('kRCX_IREchoError','LongInt').SetInt( - 2);
 CL.AddConstantN('kRCX_ReplyError','LongInt').SetInt( - 3);
 CL.AddConstantN('kRCX_RequestError','LongInt').SetInt( - 4);
 CL.AddConstantN('kRCX_FileError','LongInt').SetInt( - 5);
 CL.AddConstantN('kRCX_FormatError','LongInt').SetInt( - 6);
 CL.AddConstantN('kRCX_AbortError','LongInt').SetInt( - 7);
 CL.AddConstantN('kRCX_MemFullError','LongInt').SetInt( - 8);
 CL.AddConstantN('kRCX_PipeModeError','LongInt').SetInt( - 9);
 CL.AddConstantN('kRCX_LastError','LongInt').SetInt( - 9);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)

procedure RIRegister_rcx_constants(CL: TPSRuntimeClassImporter);
begin
  // do nothing
end;


{ TPSImport_rcx_constants }
(*----------------------------------------------------------------------------*)
procedure TPSImport_rcx_constants.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_rcx_constants(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_rcx_constants.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_rcx_constants(ri);
end;
(*----------------------------------------------------------------------------*)
 
 
end.