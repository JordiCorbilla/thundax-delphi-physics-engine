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
unit tdpe.lib.particle.abstractparticle;

interface

Uses
  tdpe.lib.vector, tdpe.lib.math.interval, tdpe.lib.collision, tdpe.lib.particle.abstractElement,
  tdpe.lib.render, FMX.Graphics, tdpe.lib.styles, tdpe.lib.styles.contract, Classes, tdpe.lib.force.contract,
  tdpe.lib.writer.contract, tdpe.lib.particle.physicElement, tdpe.lib.force.list,
  tdpe.lib.collision.contract, Generics.Collections, System.UITypes, System.Types;

Type
  TAbstractParticle = Class(TAbstractElement)
  private
    FCollision: ICollision;
    FElasticity: Double;
    Fmass: Double;
    FinvertedOfMass: Double;
    FFriction: Double;
    FFixed: Boolean;
    FCollidable: Boolean;
    FCenter: IVector;
    FCollision_vn: IVector;
    FCollision_vt: IVector;
    FMultiSample: Integer;
    FparticleColor: TColor;
    FStyle: IStyle;
    FLogger: IWriter;
    FSample: IVector;
    FInterval: TInterval;
    FForces: IVector;
    FTemporal: IVector;
    Fmtdres: IVector;
    FCurrent: IVector;
    FPrevious: IVector;
    FForceList: TForceList;
    procedure SetStyle(const Value: IStyle);
  protected
    FRenderer: TAbstractRenderer;
  public
    function GetParticleColor: TColor; virtual;
    procedure SetParticleColor(const Value: TColor); Virtual;
    function GetFriction: Double; virtual;
    procedure SetFriction(const Value: Double); Virtual;
    function GetElasticity: Double; virtual;
    procedure SetElasticity(const Value: Double); virtual;
    function GetMass: Double; virtual;
    procedure SetMass(Value: Double); Virtual;
    Function GetVelocity: IVector; Virtual;
    Procedure SetVelocity(Value: IVector); Virtual;
    Function getPx: Double; Virtual;
    Procedure SetPx(Value: Double); Virtual;
    Function getPy: Double; Virtual;
    Procedure setPy(Value: Double); Virtual;
    procedure setLog(const Value: IWriter);
    Constructor Create(x, y, mass, Elasticity, Friction: Double; IsFixed: Boolean; color: TColor); Reintroduce; Virtual;
    Destructor Destroy; Override;
    procedure ClearForces();
    procedure SetRenderer(aRenderer: TAbstractRenderer);
    Procedure CleanUp; Override;
    Function GetInvMass: Double; Virtual;
    Function Center: IVector;
    Function Position: IVector; Overload;
    Procedure Position(Value: IVector); Overload;
    function GetCollision: ICollision;
    procedure UpdateGeometricState(deltaTime: Double; MasslessForce: TForceList; Damping: Double); Virtual;
    procedure accumulateForces(aForceList: TForceList);
    procedure AddForce(force: IForce);
    function GetComponents(CollisionNormal: IVector): ICollision;
    procedure ResolveCollision(mtd, vel, n: IVector; d: Double; o: Integer; particle: TAbstractParticle); Virtual;
    function ParticleType: Integer;
    property ParticleColor: TColor read GetParticleColor write SetParticleColor;
    property px: Double read getPx write SetPx;
    property py: Double read getPy Write setPy;
    property Fixed: Boolean read FFixed Write FFixed;
    property Style: IStyle read FStyle write SetStyle;
    property Log: IWriter read FLogger;
    property mass: Double read GetMass Write SetMass;
    property Velocity: IVector read GetVelocity Write SetVelocity;
    property Collidable: Boolean read FCollidable Write FCollidable;
    Property Elasticity: Double Read GetElasticity Write SetElasticity;
    Property Friction: Double read GetFriction Write SetFriction;
    Property MultiSample: Integer read FMultiSample Write FMultiSample;
    Property InvMass: Double read GetInvMass;
    property Sample: IVector read FSample write FSample;
    property interval: TInterval read FInterval write FInterval;
    property Renderer: TAbstractRenderer read FRenderer;
    property Forces: IVector read FForces write FForces;
    property Temporal: IVector read FTemporal write FTemporal;
    property mtdres: IVector read Fmtdres write Fmtdres;
    property Current: IVector read FCurrent write FCurrent;
    property Previous: IVector read FPrevious write FPrevious;
    property ForceList: TForceList read FForceList write FForceList;
    class function New(x, y, mass, Elasticity, Friction: Double; IsFixed: Boolean; color: TColor): TAbstractParticle;
  end;

implementation

uses SysUtils;

{ TAbstractParticle }

procedure TAbstractParticle.accumulateForces(aForceList: TForceList);
begin
  FForceList.accumulateForces(FForces, FinvertedOfMass);
  aForceList.accumulateForces(FForces, FinvertedOfMass);
end;

procedure TAbstractParticle.AddForce(force: IForce);
begin
  FForceList.Add(force);
end;

function TAbstractParticle.Center: IVector;
begin
  FCenter.SetPoint(px, py);
  Result := FCenter;
end;

procedure TAbstractParticle.CleanUp;
begin
  inherited;
end;

procedure TAbstractParticle.ClearForces;
begin
  if Assigned(FForceList) then
    FreeAndNil(FForceList);
  FForceList := TForceList.Create;
  FForces.SetPoint(0, 0);
end;

constructor TAbstractParticle.Create(x, y, mass, Elasticity, Friction: Double; IsFixed: Boolean; color: TColor);
begin
  inherited Create;
  FInterval := TInterval.Create(0, 0);
  FForceList := TForceList.Create;
  FStyle := TStyle.Create();
  FStyle.BrushColor := color;
  FCurrent := TVector.New.SetPoint(x, y);
  FPrevious := TVector.New.SetPoint(x, y);
  FSample := TVector.New;
  FTemporal := TVector.New;
  Fixed := IsFixed;
  FparticleColor := color;
  Forces := TVector.New;
  Fmtdres := TVector.New;
  FCollision_vn := TVector.New;
  FCollision_vt := TVector.New;
  FCollision := TCollision.Create(FCollision_vn, FCollision_vt);
  FCollidable := true;
  SetMass(mass);
  SetElasticity(Elasticity);
  SetFriction(Friction);
  FCenter := TVector.New;
  FMultiSample := 0;
end;

destructor TAbstractParticle.Destroy;
begin
  FreeAndNil(FInterval);
  FreeAndNil(FForceList);
  FCollision := nil;
  FStyle := nil;
end;

function TAbstractParticle.GetComponents(CollisionNormal: IVector): ICollision;
var
  vel: IVector;
  vdotn: Double;
begin
  vel := Velocity;
  vdotn := CollisionNormal.InnerProduct(vel);

  FCollision.normal := CollisionNormal.ScalarProduct(vdotn);
  FCollision.Velocity := vel.Substract(FCollision.normal);
  Result := FCollision;
end;

function TAbstractParticle.GetCollision(): ICollision;
begin
  Result := FCollision;
end;

function TAbstractParticle.GetInvMass: Double;
begin
  Result := FinvertedOfMass;
  if Fixed then
    Result := 0;
end;

procedure TAbstractParticle.SetMass(Value: Double);
begin
  If Value <= 0 then
    Raise Exception.Create('Mass must be not less or equals than 0.');
  Fmass := Value;
  FinvertedOfMass := 1 / Fmass;
end;

function TAbstractParticle.ParticleType: Integer;
begin
  Result := 0;
end;

function TAbstractParticle.Position: IVector;
begin
  Result := TVector.New.SetPoint(FCurrent.x, FCurrent.y);
end;

procedure TAbstractParticle.Position(Value: IVector);
begin
  FCurrent.Clone(Value);
  FPrevious.Clone(Value);
end;

procedure TAbstractParticle.SetParticleColor(const Value: TColor);
begin
  FparticleColor := Value;
end;

procedure TAbstractParticle.SetPx(Value: Double);
begin
  FCurrent.x := Value;
  FPrevious.x := Value;
end;

function TAbstractParticle.getPx: Double;
begin
  Result := FCurrent.x;
end;

procedure TAbstractParticle.setPy(Value: Double);
begin
  FCurrent.y := Value;
  FPrevious.y := Value;
end;

function TAbstractParticle.getPy: Double;
begin
  Result := FCurrent.y;
end;

procedure TAbstractParticle.ResolveCollision(mtd, vel, n: IVector; d: Double; o: Integer; particle: TAbstractParticle);
begin
  if (Fixed) or (not Solid) or (not particle.Solid) then
    Exit;

  Fmtdres.Clone(mtd);
  FCurrent.Clone(FSample);
  FCurrent.Add(mtd, true);
  Velocity := vel;
end;

procedure TAbstractParticle.SetVelocity(Value: IVector);
begin
  FPrevious := FCurrent.Substract(Value);
end;

function TAbstractParticle.GetVelocity: IVector;
begin
  Result := FCurrent.Substract(FPrevious);
end;

class function TAbstractParticle.New(x, y, mass, Elasticity, Friction: Double; IsFixed: Boolean; color: TColor): TAbstractParticle;
begin
  Result := Create(x, y, mass, Elasticity, Friction, IsFixed, color);
end;

procedure TAbstractParticle.UpdateGeometricState(deltaTime: Double; MasslessForce: TForceList; Damping: Double);
var
  newVector: IVector;
begin
  if (Fixed) then
    Exit;
  accumulateForces(MasslessForce);
  FTemporal.Clone(FCurrent);
  newVector := Velocity.Add(FForces.ScalarProduct(deltaTime, true));
  FCurrent.Add(newVector.ScalarProduct(Damping, true), true);
  FPrevious.Clone(Temporal);
  ClearForces();
end;

procedure TAbstractParticle.SetRenderer(aRenderer: TAbstractRenderer);
begin
  FRenderer := aRenderer;
end;

procedure TAbstractParticle.SetStyle(const Value: IStyle);
begin
  FStyle := Value;
end;

function TAbstractParticle.GetMass: Double;
begin
  Result := Fmass;
end;

function TAbstractParticle.GetParticleColor: TColor;
begin
  Result := FparticleColor;
end;

function TAbstractParticle.GetElasticity: Double;
begin
  Result := FElasticity
end;

procedure TAbstractParticle.SetElasticity(const Value: Double);
begin
  FElasticity := Value;
end;

function TAbstractParticle.GetFriction: Double;
begin
  Result := FFriction;
end;

procedure TAbstractParticle.SetFriction(const Value: Double);
begin
  FFriction := Value;
end;

procedure TAbstractParticle.setLog(const Value: IWriter);
begin
  FLogger := Value;
end;

end.
