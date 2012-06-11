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
unit fObjectDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tdpe.lib.engine, tdpe.lib.vector, tdpe.lib.render.gdi,
  tdpe.lib.structures.car, tdpe.lib.structures.capsule,
  tdpe.lib.automation.swingdoor, tdpe.lib.automation.rotator, ExtCtrls,
  tdpe.lib.structures.surface.static, StdCtrls;

type
  TFormMainCarDemo = class(TForm)
    tmr1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    Engine: TEngine;
    Render: TGDIRenderer;
    FCar: TCar;
    FCapsule: TCapsule;
    FSwingDoor: TSwingDoor;
    FRotator: TRotator;
    Fsurface: TSurfaces;
  end;

var
  FormMainCarDemo: TFormMainCarDemo;

implementation

uses
  tdpe.lib.force;

{$R *.dfm}

procedure TFormMainCarDemo.FormCreate(Sender: TObject);
begin
  Engine := TEngine.Create(1 / 4);
  Render := TGDIRenderer.Create(FormMainCarDemo.Canvas, ClientRect);

  Engine.AddForce(TForce.Create(false, 0, 1));
  Engine.damping := 1;
  Engine.RestrictionCollisionCycles := 1;

  Fsurface := TSurfaces.Create(Render, Engine, nil);
  Engine.addGroup(Fsurface);

  FCapsule := TCapsule.Create(Render, Engine);
  Engine.addGroup(FCapsule);

  FRotator := TRotator.Create(Render, Engine);
  Engine.addGroup(FRotator);

  FCar := TCar.Create(Render, Engine);
  Engine.addGroup(FCar);

  FCar.AddCollidable(FCapsule);
  FCar.AddCollidable(FRotator);

  Fsurface.AddCollidable(FCar);
  Fsurface.AddCollidable(FCapsule);

  FCapsule.AddCollidable(FRotator);

  DoubleBuffered := true;
end;

procedure TFormMainCarDemo.tmr1Timer(Sender: TObject);
begin
  Engine.Run;
  FRotator.RotateByRadian(0.02);
  Repaint;
end;

procedure TFormMainCarDemo.FormPaint(Sender: TObject);
begin
  Engine.Paint;
end;

procedure TFormMainCarDemo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = 'a') or (Key = 'A') then
    FCar.Speed := FCar.Speed - 0.2;
  if (Key = 'd') or (Key = 'D') then
    FCar.Speed := FCar.Speed + 0.2;
end;

procedure TFormMainCarDemo.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FCar.Speed := 0;
end;

end.
