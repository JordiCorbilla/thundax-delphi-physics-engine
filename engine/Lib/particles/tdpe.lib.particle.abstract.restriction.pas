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
unit tdpe.lib.particle.abstract.restriction;

interface

Uses tdpe.lib.particle.abstractElement, tdpe.lib.render;

type

  TAbstractRestriction = Class(TAbstractElement)
  Private
    Fstiffness: double;
    FRenderer: TAbstractRenderer;
  Public
    constructor Create(Stiffness: double); Reintroduce;
    Function Stiffness: double; Overload;
    Procedure Stiffness(Value: double); Overload;
    Procedure Init; Override;
    Procedure SetRenderer(aRenderer: TAbstractRenderer);
    Procedure Resolve; Virtual; Abstract;
    property Renderer: TAbstractRenderer read FRenderer;
  end;

implementation

{ AbstractRestriction }

constructor TAbstractRestriction.Create(Stiffness: double);
begin
  Inherited Create;
  Self.Stiffness(Stiffness);
end;

function TAbstractRestriction.Stiffness: double;
begin
  Result := Fstiffness;
end;

procedure TAbstractRestriction.Init;
begin
  inherited;
end;

procedure TAbstractRestriction.SetRenderer(aRenderer: TAbstractRenderer);
begin
  FRenderer := aRenderer;
end;

procedure TAbstractRestriction.Stiffness(Value: double);
begin
  Fstiffness := Value;
end;

end.
