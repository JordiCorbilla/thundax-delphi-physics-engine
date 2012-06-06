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
unit tdpe.lib.structures.dispenser.box;

interface

uses
  tdpe.lib.particle.group, Contnrs, Generics.Collections, tdpe.lib.render, Graphics,
  tdpe.lib.engine, tdpe.lib.particle.box.solid, tdpe.lib.writer.contract;

type
  TBoxDispenser = class(TGroup)
  private
    FBoxes: TObjectList<TSolidBox>;
    FRenderer: TAbstractRenderer;
    FProductColor: TColor;
    Fposx: integer;
    Fposy: integer;
    FLog: IWriter;
  public
    Constructor Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: integer; color: TColor); Reintroduce;
    procedure setLog(const value: IWriter);
    destructor Destroy(); override;
    procedure AddBox();
    function isInside(x, y: integer; var pos: integer): boolean;
    procedure Activate(i: integer);
    procedure Deactivate();
    procedure Move(x, y: integer);
  end;

implementation

uses
  tdpe.lib.particle.abstract.collection, SysUtils, tdpe.lib.colour.helper;

{ TBoxDispenser }

procedure TBoxDispenser.Activate(i: integer);
begin
  FBoxes[i].StartDrag := true;
end;

procedure TBoxDispenser.AddBox;
var
  newBox: TSolidBox;
begin
  if FBoxes.Count < 100 then
  begin
    newBox := TSolidBox.Create(Fposx, Fposy, 40, 40, 0, false, 1, 0.3, 0.8, 0.2, TRandomColor.getRandomColor);
    newBox.setLog(FLog);
    newBox.SetRenderer(FRenderer);
    AddParticle(newBox);
    FBoxes.Add(newBox);
  end;
end;

constructor TBoxDispenser.Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: integer; color: TColor);
begin
  inherited Create(true);
  FRenderer := aRenderer;
  FProductColor := color;
  Fposx := px;
  Fposy := py;
  FBoxes := TObjectList<TSolidBox>.Create;
end;

procedure TBoxDispenser.Deactivate;
var
  box: TSolidBox;
begin
  for box in FBoxes do
    box.StartDrag := false;
end;

destructor TBoxDispenser.Destroy;
begin
  FreeAndNil(FBoxes);
  inherited;
end;

function TBoxDispenser.isInside(x, y: integer; var pos: integer): boolean;
var
  i: integer;
  bFound: boolean;
begin
  i := 0;
  bFound := false;
  while (i < FBoxes.Count) and (not bFound) do
  begin
    bFound := FBoxes[i].isInside(x, y);
    pos := i;
    Inc(i);
  end;
  result := bFound;
end;

procedure TBoxDispenser.Move(x, y: integer);
var
  box: TSolidBox;
begin
  for box in FBoxes do
    if box.StartDrag then
      box.Move(x, y);
end;

procedure TBoxDispenser.setLog(const value: IWriter);
begin
  FLog := value;
end;

end.
