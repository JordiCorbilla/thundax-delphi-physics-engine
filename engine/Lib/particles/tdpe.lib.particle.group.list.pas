(*
 * Copyright (c) 2010-2012 Thundax Delphi Physics Engine
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name of 'TDPE' nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
unit tdpe.lib.particle.group.list;

interface

uses
  tdpe.lib.particle.group, Generics.Collections, tdpe.lib.force.contract, tdpe.lib.render.contract, tdpe.lib.force.list;

type
  TGroupList = class(TList<TGroup>, IInterface, IDrawable)
  private
    FRefCount: Integer;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CheckCollisions;
    Procedure Integrate(deltaTime: Double; MassLessForce: TForceList; Damping: Double);
    procedure Paint;
    procedure SatisfyRestrictions;
  end;

implementation

//uses
//  Windows;

{ TGroupList }

function TGroupList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TGroupList._AddRef: Integer;
begin
  //Result := InterlockedIncrement(FRefCount);
end;

function TGroupList._Release: Integer;
begin
  //Result := InterlockedDecrement(FRefCount);
  //if Result = 0 then
  //  Destroy;
end;

procedure TGroupList.CheckCollisions;
var
  g: TGroup;
begin
  for g in Self do
    g.CheckCollision;
end;

procedure TGroupList.Integrate(deltaTime: Double; MassLessForce: TForceList; Damping: Double);
var
  g: TGroup;
begin
  for g in Self do
    g.Integrate(deltaTime, MassLessForce, Damping);
end;

procedure TGroupList.Paint;
var
  g: TGroup;
begin
  for g in Self do
    g.Paint;
end;

procedure TGroupList.SatisfyRestrictions;
var
  g: TGroup;
begin
  for g in Self do
    g.SatisfyRestrictions;
end;

end.
