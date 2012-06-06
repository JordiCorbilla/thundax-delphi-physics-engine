unit tdpe.lib.particle.physicElement;

interface

uses
  tdpe.lib.vector, Graphics, tdpe.lib.styles.contract, tdpe.lib.writer.contract;

type
  TPhysicElement = class(TObject)
  private
    FFixed: Boolean;
    FStyle: IStyle;
    FLogger: IWriter;
    FCollidable: Boolean;
    FMultiSample: Integer;
    FElasticity: Double;
    FFriction: Double;
    FCurrent: IVector;
    FPrevious: IVector;
    FparticleColor: TColor;
    Fmass: Double;
    FinvertedOfMass: Double;
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
    procedure SetStyle(const Value: IStyle);
    Function GetInvMass: Double; Virtual;
  public
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
    property ParticleColor: TColor read GetParticleColor write SetParticleColor;
    constructor Create(Current, Previous : IVector);
  end;

implementation

uses
  SysUtils;

{ TPhysicElement }

constructor TPhysicElement.Create(Current, Previous: IVector);
begin
  FCurrent := Current;
  FPrevious := Previous;
end;

function TPhysicElement.GetElasticity: Double;
begin
  Result := FElasticity;
end;

function TPhysicElement.GetFriction: Double;
begin
  Result := FFriction;
end;

function TPhysicElement.GetInvMass: Double;
begin
  Result := FinvertedOfMass;
  if Fixed then
    Result := 0;
end;

function TPhysicElement.GetMass: Double;
begin
  Result := Fmass;
end;

function TPhysicElement.GetParticleColor: TColor;
begin
  result := FparticleColor;
end;

function TPhysicElement.getPx: Double;
begin
  Result := FCurrent.x;
end;

function TPhysicElement.getPy: Double;
begin
  Result := FCurrent.y;
end;

function TPhysicElement.GetVelocity: IVector;
begin
  Result := FCurrent.Substract(FPrevious);
end;

procedure TPhysicElement.SetElasticity(const Value: Double);
begin
  FElasticity := Value;
end;

procedure TPhysicElement.SetFriction(const Value: Double);
begin
  FFriction := Value;
end;

procedure TPhysicElement.SetMass(Value: Double);
begin
  If Value <= 0 then
    Raise Exception.Create('Mass must be not less or equals than 0.');
  Fmass := Value;
  FinvertedOfMass := 1 / Fmass;
end;

procedure TPhysicElement.SetParticleColor(const Value: TColor);
begin
  FparticleColor := Value;
end;

procedure TPhysicElement.SetPx(Value: Double);
begin
  FCurrent.x := Value;
  FPrevious.x := Value;
end;

procedure TPhysicElement.setPy(Value: Double);
begin
  FCurrent.y := Value;
  FPrevious.y := Value;
end;

procedure TPhysicElement.SetStyle(const Value: IStyle);
begin
  FStyle := Value;
end;

procedure TPhysicElement.SetVelocity(Value: IVector);
begin
  FPrevious := FCurrent.Substract(Value);
end;

end.
