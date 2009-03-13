(*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of this code is John Hansen.
 * Portions created by John Hansen are Copyright (C) 2009 John Hansen.
 * All Rights Reserved.
 *
 *)
unit uPSI_brick_common;
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
  TPSImport_brick_common = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_brick_common(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_brick_common_Routines(S: TPSExec);

procedure Register;

implementation


uses
   uSpirit
  ,brick_common
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_brick_common]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_brick_common(CL: TPSPascalCompiler);
begin
 CL.AddDelphiFunction('Function BrickComm : TBrickComm');
 CL.AddDelphiFunction('Procedure ReleaseBrickComm');
 CL.AddDelphiFunction('Procedure CreateInitFile');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_brick_common_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@BrickComm, 'BrickComm', cdRegister);
 S.RegisterDelphiFunction(@ReleaseBrickComm, 'ReleaseBrickComm', cdRegister);
 S.RegisterDelphiFunction(@CreateInitFile, 'CreateInitFile', cdRegister);
end;

 
 
{ TPSImport_brick_common }
(*----------------------------------------------------------------------------*)
procedure TPSImport_brick_common.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_brick_common(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_brick_common.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
//  RIRegister_brick_common(ri);
  RIRegister_brick_common_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)
 
 
end.
