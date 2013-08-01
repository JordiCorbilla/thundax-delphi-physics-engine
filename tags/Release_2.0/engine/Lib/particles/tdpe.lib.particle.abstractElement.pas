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
unit tdpe.lib.particle.abstractElement;

interface

uses
  tdpe.lib.particle.abstractElement.contract, tdpe.lib.render.contract;

Type
  TAbstractElement = class(TInterfacedObject, IElement, IDrawable)
  private
    FVisible: Boolean;
    FAlwaysRepaint: Boolean;
    FSolid: Boolean;
    procedure SetAlwaysRepaint(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetSolid(const Value: Boolean);
  Public
    Constructor Create; Virtual;
    Procedure Init; Virtual; Abstract;
    Procedure Paint; Virtual; Abstract;
    Procedure CleanUp; Virtual; Abstract;
    Property Visible: Boolean read FVisible Write SetVisible;
    Property AlwaysRepaint: Boolean read FAlwaysRepaint Write SetAlwaysRepaint;
    property Solid: Boolean read FSolid write SetSolid;
  end;

implementation

{ TAbstractElement }

constructor TAbstractElement.Create;
begin
  FVisible := true;
  FSolid := true;
  FAlwaysRepaint := False;
end;

procedure TAbstractElement.SetAlwaysRepaint(const Value: Boolean);
begin
  FAlwaysRepaint := Value;
end;

procedure TAbstractElement.SetSolid(const Value: Boolean);
begin
  FSolid := Value;
end;

procedure TAbstractElement.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

end.
