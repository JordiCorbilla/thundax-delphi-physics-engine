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
unit tdpe.lib.particle.spring.restriction;

interface

uses tdpe.lib.particle.abstract.restriction, tdpe.lib.particle.abstractparticle, tdpe.lib.math.helper,
  tdpe.lib.vector, math, SysUtils, Graphics;

Type
  TSpringRestriction = Class(TAbstractRestriction)
  Private
    particle1: TAbstractParticle;
    particle2: TAbstractParticle;
    FrestLength: Double;
    Fcollidable: Boolean;
    FRestrictionParticle: TAbstractParticle;
    Fcolor: TColor;
    FPenWidth: Integer;
    function GetCurrLenght: Double;
    function GetCenter: IVector;
    function GetAngle: Double;
    function GetRadian: Double;
    function GetRectHeight: Double;
    function GetRectScale: Double;
    function GetDelta: IVector;
    procedure SetRestLength(const Value: Double);
    function GetFixedEndLimit: Double;
    procedure SetFixedEndLimit(const Value: Double);
    function GetFixed: Boolean;
  Public
    constructor Create(particle1, particle2: TAbstractParticle; Stiffness: Double = 0.5; Collidable: Boolean = False; rectHeight: Double = 1; rectScale: Double = 1; ScaleToLEngth: Boolean = False;
      color: TColor = clBlack; PenWidth: Integer = 1);
    destructor Destroy(); override;
    procedure SetStyle(color: TColor; PenWidth: Integer);
    Procedure checkParticlesLocation;
    Procedure Resolve; Override;
    procedure SetCollidable(collidable: Boolean; aRectHeight, aRectScale: Double; ScaleLen: Boolean = False);
    Function IsConnectedTo(particle: TAbstractParticle): Boolean;
    Procedure CleanUp; Override;
    Procedure Init; Override;
    Procedure Paint; OverridE;
    Property CurrLength: Double read GetCurrLenght;
    property Center: IVector read GetCenter;
    property Angle: Double read GetAngle;
    property Radian: Double Read GetRadian;
    property rectScale: Double read GetRectScale;
    Property rectHeight: Double Read GetRectHeight;
    property Delta: IVector read GetDelta;
    property RestLength: Double read FrestLength Write SetRestLength;
    property Collidable: Boolean read Fcollidable Write Fcollidable;
    property SCP: TAbstractParticle read FRestrictionParticle;
    property FixedEndLimit: Double read GetFixedEndLimit Write SetFixedEndLimit;
    property Fixed: Boolean read GetFixed;
  end;

implementation

uses tdpe.lib.particle.spring, tdpe.lib.particle.abstractElement, tdpe.lib.math;

{ TSpringRestriction }


procedure TSpringRestriction.checkParticlesLocation;
begin
  //if the two particles are at the same location offset slightly
  if ((TMathHelper.Compare(particle1.current.x, particle2.current.x, '=')) and (TMathHelper.Compare(particle1.current.y, particle2.current.y, '='))) Then
    particle2.current.x := particle2.current.x + 0.0001;
end;

procedure TSpringRestriction.CleanUp;
begin
  inherited;

end;

constructor TSpringRestriction.Create(particle1, particle2: TAbstractParticle; Stiffness: Double; Collidable: Boolean; rectHeight, rectScale: Double; ScaleToLEngth: Boolean; color: TColor; PenWidth: Integer);
begin
  inherited Create(Stiffness);
  Self.particle1 := particle1;
  Self.particle2 := particle2;
  Self.Fcolor := color;
  Self.FPenWidth := PenWidth;
  checkParticlesLocation;
  FrestLength := CurrLength;
  SetCollidable(Collidable, rectHeight, rectScale, ScaleToLEngth);
end;

destructor TSpringRestriction.Destroy;
begin
  if Assigned(FRestrictionParticle) then
    FreeAndNil(FRestrictionParticle);
  inherited;
end;

function TSpringRestriction.GetAngle: Double;
begin
  Result := Radian * ONE_EIGHTY_OVER_PI;
end;

function TSpringRestriction.GetCenter: IVector;
begin
  Result := (particle1.current.Add(particle2.current)).Divide(2);
end;

function TSpringRestriction.GetCurrLenght: Double;
begin
  Result := particle1.current.Distance(particle2.current);
end;

function TSpringRestriction.GetDelta: IVector;
begin
  Result := particle1.current.Substract(particle2.current);
end;

function TSpringRestriction.GetFixed: Boolean;
begin
  Result := particle1.Fixed And particle2.Fixed;
end;

function TSpringRestriction.GetFixedEndLimit: Double;
begin
  Result := TSpringRestrictionParticle(FRestrictionParticle).FixedEndLimit;
end;

function TSpringRestriction.GetRadian: Double;
var
  deltaVector: IVector;
begin
  deltaVector := Delta;
  Result := Arctan2(deltaVector.y, deltaVector.x);
end;

function TSpringRestriction.GetRectHeight: Double;
begin
  Result := TSpringRestrictionParticle(FRestrictionParticle).rectHeight;
end;

function TSpringRestriction.GetRectScale: Double;
begin
  Result := TSpringRestrictionParticle(FRestrictionParticle).rectScale;
end;

procedure TSpringRestriction.Init;
begin
  inherited;

end;

function TSpringRestriction.IsConnectedTo(particle: TAbstractParticle): Boolean;
begin
  Result := (particle = particle1) Or (particle = particle2);
end;

procedure TSpringRestriction.Paint;
begin
  if Self.Visible then
  begin
    if Collidable then
    begin
      FRestrictionParticle.ParticleColor := Self.Fcolor;
      TSpringRestrictionParticle(FRestrictionParticle).Paint;
    end
    else
      Renderer.Line(particle1.px, particle1.py, particle2.px, particle2.py, Self.Fcolor, Self.FPenWidth);
  end;
end;

procedure TSpringRestriction.Resolve;
var
  DeltaLength: Double;
  Diff: Double;
  dmds: IVector;
begin
  if particle1.Fixed and particle2.Fixed then
    Exit;

  DeltaLength := CurrLength;
  Diff := (DeltaLength - RestLength) / (DeltaLength * (particle1.invMass + particle2.invMass));
  dmds := Delta.ScalarProduct(Diff * Stiffness);

  particle1.current.Substract(dmds.ScalarProduct(particle1.invMass), true);
  particle2.current.Add(dmds.ScalarProduct(particle2.invMass), true);
end;

procedure TSpringRestriction.SetCollidable(collidable: Boolean; aRectHeight, aRectScale: Double; ScaleLen: Boolean);
begin
  Fcollidable := collidable;
  if Assigned(FRestrictionParticle) then
    FreeAndNil(FRestrictionParticle);
  if Fcollidable then
  begin
    FRestrictionParticle := TSpringRestrictionParticle.Create(particle1, particle2, Self, aRectHeight, aRectScale, ScaleLen, Self.Fcolor);
  end;
end;

procedure TSpringRestriction.SetFixedEndLimit(const Value: Double);
begin
  if Assigned(FRestrictionParticle) then
    TSpringRestrictionParticle(FRestrictionParticle).FixedEndLimit := Value;
end;

procedure TSpringRestriction.SetRestLength(const Value: Double);
begin
  if Value <= 0 then
    raise Exception.Create('SpringConstant.RestLength must be grater than 0');
  FrestLength := Value;
end;

procedure TSpringRestriction.SetStyle(color: TColor; PenWidth: Integer);
begin
  Self.Fcolor := color;
  Self.FPenWidth := PenWidth;
end;

end.
