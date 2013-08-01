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
unit fPlantDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, StdCtrls,

  // TDPE
  tdpe.lib.engine, tdpe.lib.structures.car,
  tdpe.lib.vector, tdpe.lib.render.gdi, tdpe.lib.engine.wrapper,
  tdpe.lib.automation.valve, tdpe.lib.automation.product,
  tdpe.lib.automation.silo, tdpe.lib.automation.scale;

type
  TFormMainPlantDemo = class(TForm)
    tmr1: TTimer;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    btn1: TSpeedButton;
    Button2: TButton;
    Button6: TButton;
    Button7: TButton;
    SpeedButton1: TSpeedButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    SpeedButton2: TSpeedButton;
    Button1: TButton;
    Button11: TButton;
    Button12: TButton;
    SpeedButton3: TSpeedButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    SpeedButton4: TSpeedButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    SpeedButton5: TSpeedButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    SpeedButton6: TSpeedButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    SpeedButton7: TSpeedButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    SpeedButton8: TSpeedButton;
    Button28: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    tmr2: TTimer;
    tmr3: TTimer;
    tmr4: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmr2Timer(Sender: TObject);
    procedure tmr3Timer(Sender: TObject);
    procedure tmr4Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    engine: TFluentEngine;
    render: TGDIRenderer;
    aValve1, aValve2, aValve3, aValve4, aValve5, aValve6, aValve7, aValve8, aValve9: TValve;
    startv1, openv1, closev1: Boolean;
    startv2, openv2, closev2: Boolean;
    startv3, openv3, closev3: Boolean;
    startv4, openv4, closev4: Boolean;
    startv5, openv5, closev5: Boolean;
    startv6, openv6, closev6: Boolean;
    startv7, openv7, closev7: Boolean;
    startv8, openv8, closev8: Boolean;
    startv9, openv9, closev9: Boolean;
    aProduct1, aProduct2, aProduct3, aProduct4, aProduct5, aProduct6, aProduct7, aProduct8, aProduct9: TProduct;
    aSilo1, aSilo2, aSilo3, aSilo4, aSilo5, aSilo6, aSilo7, aSilo8, aSilo9: TSilo;
    aScale1, aScale2, aScale3: TScale;
    scale1open, scale2open, scale3open: Boolean;
    scale1close, scale2close, scale3close: Boolean;
    initVector: IVector;
  end;

var
  FormMainPlantDemo: TFormMainPlantDemo;

implementation

uses
  tdpe.lib.force;
{$R *.dfm}

procedure TFormMainPlantDemo.btn1Click(Sender: TObject);
begin
  tmr2.Enabled := not tmr2.Enabled;
end;

procedure TFormMainPlantDemo.Button10Click(Sender: TObject);
begin
  openv3 := false;
  closev3 := true;
  startv3 := false;
end;

procedure TFormMainPlantDemo.Button11Click(Sender: TObject);
begin
  openv4 := false;
  closev4 := false;
  startv4 := true;
end;

procedure TFormMainPlantDemo.Button12Click(Sender: TObject);
begin
  openv4 := false;
  closev4 := true;
  startv4 := false;
end;

procedure TFormMainPlantDemo.Button13Click(Sender: TObject);
begin
  openv5 := true;
  closev5 := false;
  startv5 := false;
end;

procedure TFormMainPlantDemo.Button14Click(Sender: TObject);
begin
  openv5 := false;
  closev5 := false;
  startv5 := true;
end;

procedure TFormMainPlantDemo.Button15Click(Sender: TObject);
begin
  openv5 := false;
  closev5 := true;
  startv5 := false;
end;

procedure TFormMainPlantDemo.Button16Click(Sender: TObject);
begin
  openv6 := true;
  closev6 := false;
  startv6 := false;
end;

procedure TFormMainPlantDemo.Button17Click(Sender: TObject);
begin
  openv6 := false;
  closev6 := false;
  startv6 := true;
end;

procedure TFormMainPlantDemo.Button18Click(Sender: TObject);
begin
  openv6 := false;
  closev6 := true;
  startv6 := false;
end;

procedure TFormMainPlantDemo.Button19Click(Sender: TObject);
begin
  openv7 := true;
  closev7 := false;
  startv7 := false;
end;

procedure TFormMainPlantDemo.Button1Click(Sender: TObject);
begin
  openv4 := true;
  closev4 := false;
  startv4 := false;
end;

procedure TFormMainPlantDemo.Button20Click(Sender: TObject);
begin
  openv7 := false;
  closev7 := false;
  startv7 := true;
end;

procedure TFormMainPlantDemo.Button21Click(Sender: TObject);
begin
  openv7 := false;
  closev7 := true;
  startv7 := false;
end;

procedure TFormMainPlantDemo.Button22Click(Sender: TObject);
begin
  openv8 := true;
  closev8 := false;
  startv8 := false;
end;

procedure TFormMainPlantDemo.Button23Click(Sender: TObject);
begin
  openv8 := false;
  closev8 := false;
  startv8 := true;
end;

procedure TFormMainPlantDemo.Button24Click(Sender: TObject);
begin
  openv8 := false;
  closev8 := true;
  startv8 := false;
end;

procedure TFormMainPlantDemo.Button25Click(Sender: TObject);
begin
  openv9 := true;
  closev9 := false;
  startv9 := false;
end;

procedure TFormMainPlantDemo.Button26Click(Sender: TObject);
begin
  openv9 := false;
  closev9 := false;
  startv9 := true;
end;

procedure TFormMainPlantDemo.Button27Click(Sender: TObject);
begin
  openv9 := false;
  closev9 := true;
  startv9 := false;
end;

procedure TFormMainPlantDemo.Button28Click(Sender: TObject);
begin
  scale1open := true;
  scale1close := false;
end;

procedure TFormMainPlantDemo.Button29Click(Sender: TObject);
begin
  scale1close := true;
  scale1open := false;
end;

procedure TFormMainPlantDemo.Button2Click(Sender: TObject);
begin
  openv2 := true;
  closev2 := false;
  startv2 := false;
end;

procedure TFormMainPlantDemo.Button30Click(Sender: TObject);
begin
  scale2open := true;
  scale2close := false;
end;

procedure TFormMainPlantDemo.Button31Click(Sender: TObject);
begin
  scale2open := false;
  scale2close := true;
end;

procedure TFormMainPlantDemo.Button32Click(Sender: TObject);
begin
  scale3open := false;
  scale3close := true;
end;

procedure TFormMainPlantDemo.Button33Click(Sender: TObject);
begin
  scale3open := true;
  scale3close := false;
end;

procedure TFormMainPlantDemo.Button3Click(Sender: TObject);
begin
  openv1 := true;
  closev1 := false;
  startv1 := false;
end;

procedure TFormMainPlantDemo.Button4Click(Sender: TObject);
begin
  openv1 := false;
  closev1 := false;
  startv1 := true;
end;

procedure TFormMainPlantDemo.Button5Click(Sender: TObject);
begin
  openv1 := false;
  closev1 := true;
  startv1 := false;
end;

procedure TFormMainPlantDemo.Button6Click(Sender: TObject);
begin
  openv2 := false;
  closev2 := false;
  startv2 := true;
end;

procedure TFormMainPlantDemo.Button7Click(Sender: TObject);
begin
  openv2 := false;
  closev2 := true;
  startv2 := false;
end;

procedure TFormMainPlantDemo.Button8Click(Sender: TObject);
begin
  openv3 := true;
  closev3 := false;
  startv3 := false;
end;

procedure TFormMainPlantDemo.Button9Click(Sender: TObject);
begin
  openv3 := false;
  closev3 := false;
  startv3 := true;
end;

procedure TFormMainPlantDemo.FormCreate(Sender: TObject);
begin
  engine := TFluentEngine.Create(1 / 4)
    .AddInitialForce(TForce.Create(false, 0, 1))
    .AddDamping(1)
    .AddRestrictionCollitionCycles(1);
  render := TGDIRenderer.Create(FormMainPlantDemo.Canvas, ClientRect);

  aScale1 := TScale.Create(render, engine, 65, 0, clMaroon);

  aScale2 := TScale.Create(render, engine, 365, 0, clMaroon);

  aScale3 := TScale.Create(render, engine, 665, 0, clMaroon);

  aSilo1 := TSilo.Create(render, engine, 50, 50, $00408080);
  aValve1 := TValve.Create(render, engine, 77, clRed);
  aProduct1 := TProduct.Create(render, engine, 60, clYellow);
  aSilo1.AddCollidable(aProduct1);
  aValve1.AddCollidable(aProduct1);
  aScale1.AddCollidable(aProduct1);

  aSilo2 := TSilo.Create(render, engine, 150, 50, $00408080);
  aValve2 := TValve.Create(render, engine, 177, clRed);
  aProduct2 := TProduct.Create(render, engine, 160, clGreen);
  aSilo2.AddCollidable(aProduct2);
  aValve2.AddCollidable(aProduct2);
  aScale1.AddCollidable(aProduct2);
  aProduct2.AddCollidable(aProduct1);

  aSilo3 := TSilo.Create(render, engine, 250, 50, $00408080);
  aValve3 := TValve.Create(render, engine, 277, clRed);
  aProduct3 := TProduct.Create(render, engine, 260, clBlue);
  aSilo3.AddCollidable(aProduct3);
  aValve3.AddCollidable(aProduct3);
  aScale1.AddCollidable(aProduct3);
  aProduct3.AddCollidable(aProduct2);
  aProduct3.AddCollidable(aProduct1);

  aProduct1.AddCollidable(aProduct2);
  aProduct1.AddCollidable(aProduct3);
  aProduct2.AddCollidable(aProduct3);

  // second group
  aSilo4 := TSilo.Create(render, engine, 350, 50, $00408080);

  aValve4 := TValve.Create(render, engine, 377, clRed);

  aProduct4 := TProduct.Create(render, engine, 360, clFuchsia);

  aProduct4.AddCollidable(aSilo4);
  aProduct4.AddCollidable(aValve4);
  aProduct4.AddCollidable(aScale2);

  aSilo5 := TSilo.Create(render, engine, 450, 50, $00408080);

  aValve5 := TValve.Create(render, engine, 477, clRed);

  aProduct5 := TProduct.Create(render, engine, 460, clMaroon);

  aProduct5.AddCollidable(aSilo5);
  aProduct5.AddCollidable(aValve5);
  aProduct5.AddCollidable(aScale2);
  aProduct5.AddCollidable(aProduct4);

  aSilo6 := TSilo.Create(render, engine, 550, 50, $00408080);

  aValve6 := TValve.Create(render, engine, 577, clRed);

  aProduct6 := TProduct.Create(render, engine, 560, clGray);

  aProduct6.AddCollidable(aSilo6);
  aProduct6.AddCollidable(aValve6);
  aProduct6.AddCollidable(aScale2);
  aProduct6.AddCollidable(aProduct5);
  aProduct6.AddCollidable(aProduct4);

  aProduct4.AddCollidable(aProduct5);
  aProduct4.AddCollidable(aProduct6);
  aProduct5.AddCollidable(aProduct6);

  // third group
  aSilo7 := TSilo.Create(render, engine, 650, 50, $00408080);

  aValve7 := TValve.Create(render, engine, 677, clRed);

  aProduct7 := TProduct.Create(render, engine, 660, clOlive);

  aProduct7.AddCollidable(aSilo7);
  aProduct7.AddCollidable(aValve7);
  aProduct7.AddCollidable(aScale3);

  aSilo8 := TSilo.Create(render, engine, 750, 50, $00408080);

  aValve8 := TValve.Create(render, engine, 777, clRed);

  aProduct8 := TProduct.Create(render, engine, 760, clNavy);

  aProduct8.AddCollidable(aSilo8);
  aProduct8.AddCollidable(aValve8);
  aProduct8.AddCollidable(aScale3);
  aProduct8.AddCollidable(aProduct7);

  aSilo9 := TSilo.Create(render, engine, 850, 50, $00408080);

  aValve9 := TValve.Create(render, engine, 877, clRed);

  aProduct9 := TProduct.Create(render, engine, 860, clLime);

  aProduct9.AddCollidable(aSilo9);
  aProduct9.AddCollidable(aValve9);
  aProduct9.AddCollidable(aScale3);
  aProduct9.AddCollidable(aProduct5);
  aProduct9.AddCollidable(aProduct7);

  aProduct7.AddCollidable(aProduct8);
  aProduct7.AddCollidable(aProduct9);
  aProduct8.AddCollidable(aProduct9);

  Engine.addGroups(aScale1)
    .addGroups(aScale2)
    .addGroups(aScale3)
    .addGroups(aSilo1)
    .addGroups(aValve1)
    .addGroups(aProduct1)
    .addGroups(aSilo2)
    .addGroups(aValve2)
    .addGroups(aProduct2)
    .addGroups(aSilo3)
    .addGroups(aValve3)
    .addGroups(aProduct3)
    .addGroups(aSilo4)
    .addGroups(aValve4)
    .addGroups(aProduct4)
    .addGroups(aSilo5)
    .addGroups(aValve5)
    .addGroups(aProduct5)
    .addGroups(aSilo6)
    .addGroups(aValve6)
    .addGroups(aProduct6)
    .addGroups(aSilo7)
    .addGroups(aValve7)
    .addGroups(aProduct7)
    .addGroups(aSilo8)
    .addGroups(aValve8)
    .addGroups(aProduct8)
    .addGroups(aSilo9)
    .addGroups(aValve9)
    .addGroups(aProduct9);

  DoubleBuffered := true;
end;

procedure TFormMainPlantDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(aProduct9);
  FreeAndNil(aProduct8);
  FreeAndNil(aProduct7);
  FreeAndNil(aProduct6);
  FreeAndNil(aProduct5);
  FreeAndNil(aProduct4);
  FreeAndNil(aProduct3);
  FreeAndNil(aProduct2);
  FreeAndNil(aProduct1);

  FreeAndNil(aSilo9);
  FreeAndNil(aSilo8);
  FreeAndNil(aSilo7);
  FreeAndNil(aSilo6);
  FreeAndNil(aSilo5);
  FreeAndNil(aSilo4);
  FreeAndNil(aSilo3);
  FreeAndNil(aSilo2);
  FreeAndNil(aSilo1);

  FreeAndNil(aValve9);
  FreeAndNil(aValve8);
  FreeAndNil(aValve7);
  FreeAndNil(aValve6);
  FreeAndNil(aValve5);
  FreeAndNil(aValve4);
  FreeAndNil(aValve3);
  FreeAndNil(aValve2);
  FreeAndNil(aValve1);

  FreeAndNil(aScale3);
  FreeAndNil(aScale2);
  FreeAndNil(aScale1);

  FreeAndNil(render);
  FreeAndNil(engine);
end;

procedure TFormMainPlantDemo.tmr1Timer(Sender: TObject);
begin
  engine.Run;
  if startv1 then
    aValve1.Dosification;
  if openv1 then
    aValve1.Open;
  if closev1 then
    aValve1.Close;

  if startv2 then
    aValve2.Dosification;
  if openv2 then
    aValve2.Open;
  if closev2 then
    aValve2.Close;

  if startv3 then
    aValve3.Dosification;
  if openv3 then
    aValve3.Open;
  if closev3 then
    aValve3.Close;

  if startv4 then
    aValve4.Dosification;
  if openv4 then
    aValve4.Open;
  if closev4 then
    aValve4.Close;

  if startv5 then
    aValve5.Dosification;
  if openv5 then
    aValve5.Open;
  if closev5 then
    aValve5.Close;

  if startv6 then
    aValve6.Dosification;
  if openv6 then
    aValve6.Open;
  if closev6 then
    aValve6.Close;

  if startv7 then
    aValve7.Dosification;
  if openv7 then
    aValve7.Open;
  if closev7 then
    aValve7.Close;

  if startv8 then
    aValve8.Dosification;
  if openv8 then
    aValve8.Open;
  if closev8 then
    aValve8.Close;

  if startv9 then
    aValve9.Dosification;
  if openv9 then
    aValve9.Open;
  if closev9 then
    aValve9.Close;

  if scale1open then
    aScale1.OpenDoor;

  if scale2open then
    aScale2.OpenDoor;

  if scale3open then
    aScale3.OpenDoor;

  if scale1close then
    aScale1.CloseDoor;

  if scale2close then
    aScale2.CloseDoor;

  if scale3close then
    aScale3.CloseDoor;
  Repaint;
end;

procedure TFormMainPlantDemo.tmr2Timer(Sender: TObject);
begin
  aProduct1.AddProduct;
end;

procedure TFormMainPlantDemo.tmr3Timer(Sender: TObject);
begin
  aProduct2.AddProduct;
end;

procedure TFormMainPlantDemo.tmr4Timer(Sender: TObject);
begin
  aProduct3.AddProduct;
end;

procedure TFormMainPlantDemo.FormPaint(Sender: TObject);
begin
  engine.Paint;
end;

procedure TFormMainPlantDemo.SpeedButton1Click(Sender: TObject);
begin
  tmr3.Enabled := not tmr3.Enabled;
end;

procedure TFormMainPlantDemo.SpeedButton2Click(Sender: TObject);
begin
  tmr4.Enabled := not tmr4.Enabled;
end;

procedure TFormMainPlantDemo.SpeedButton3Click(Sender: TObject);
begin
  aProduct4.AddProduct;
end;

procedure TFormMainPlantDemo.SpeedButton4Click(Sender: TObject);
begin
  aProduct5.AddProduct;
end;

procedure TFormMainPlantDemo.SpeedButton5Click(Sender: TObject);
begin
  aProduct6.AddProduct;
end;

procedure TFormMainPlantDemo.SpeedButton6Click(Sender: TObject);
begin
  aProduct7.AddProduct;
end;

procedure TFormMainPlantDemo.SpeedButton7Click(Sender: TObject);
begin
  aProduct8.AddProduct;
end;

procedure TFormMainPlantDemo.SpeedButton8Click(Sender: TObject);
begin
  aProduct9.AddProduct;
end;

end.
