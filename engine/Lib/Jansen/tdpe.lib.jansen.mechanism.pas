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
unit tdpe.lib.jansen.mechanism;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.circle.solid,
    tdpe.lib.particle.spring.restriction, tdpe.lib.engine, tdpe.lib.render;

Type

    TJansenMechanism = class(TGroup)
    Private

    Public
        m1: TSolidCircle;
        Constructor Create(Render: TAbstractRenderer; aEngine: TEngine); reintroduce; Virtual;
        procedure Move(x: double);

    End;

implementation

uses
    tdpe.lib.vector , Math;

{ Bridge }

{ P1
  /   |
  /      |
  P3-----P2
  |      |
  P5-----P4
  \      |
  \    |
  \ |
  P6
  |
  P7
}

constructor TJansenMechanism.Create(Render: TAbstractRenderer; aEngine: TEngine);
var
    P1, P2, P3: TSolidCircle;
    S1, S2, S3: TSpringRestriction;
    P4, p5, p6, p7: TSolidCircle;
    n1, n2, n3, n4, n5, n6: TSpringRestriction;
    ms1, ms2: TSpringRestriction;

    // Second part
    P11, P21, P31: TSolidCircle;
    S11, S21, S31: TSpringRestriction;
    P41, p51, p61, p71: TSolidCircle;
    n11, n21, n31, n41, n51, n61: TSpringRestriction;
    ms11, ms21: TSpringRestriction;
    radius: integer;
    height: integer;
    stiffness: double;
    t1, t2, t3, t4, t5, t6: TSpringRestriction;
    r : TSpringRestriction;
begin
    inherited Create(False);
    collideInternal := true;
    radius := 7;
    height := 3;
    stiffness := 1;

    P1 := TSolidCircle.Create(600, 60, radius, False);
    P1.mass := 1;
    P1.Elasticity := 0;
    P1.SetRenderer(Render);
    addParticle(P1);

    P2 := TSolidCircle.Create(600, 101.5, radius, true);
    P2.mass := 1;
    P2.Elasticity := 0;
    P2.SetRenderer(Render);
    addParticle(P2);

    P3 := TSolidCircle.Create(559.9, 100, radius, False); // temporal
    P3.mass := 1;
    P3.Elasticity := 0;
    P3.SetRenderer(Render);
    addParticle(P3);

    S1 := TSpringRestriction.Create(P1, P2, stiffness, true, height);
    S1.SetRenderer(Render);
    addRestriction(S1);

    S2 := TSpringRestriction.Create(P2, P3, stiffness, true, height);
    S2.SetRenderer(Render);
    addRestriction(S2);

    S3 := TSpringRestriction.Create(P3, P1, stiffness, true, height);
    S3.SetRenderer(Render);
    addRestriction(S3);

    P4 := TSolidCircle.Create(600, 139.3, radius, False);
    P4.Elasticity := 0;
    P4.SetRenderer(Render);
    addParticle(P4);

    p5 := TSolidCircle.Create(563.3, 139.3, radius, False); // temporal
    P5.Elasticity := 0;
    p5.SetRenderer(Render);
    addParticle(p5);

    p6 := TSolidCircle.Create(600, 188.9, radius, False, 1); // temporal
    P6.Elasticity := 0;
    //p6.PrintTrace := true;
    p6.SetRenderer(Render);
    addParticle(p6);

//     P7 := TCircleParticle.Create(600, 220, 7, false);   //temporal
//    P7.Elasticity := 0;
//    p7.PrintTrace := true;
//     P7.SetRenderer(Render);
//     addParticle(P7);

    n1 := TSpringRestriction.Create(P2, P4, stiffness, true, height);
    n1.SetRenderer(Render);
    addRestriction(n1);

    n2 := TSpringRestriction.Create(P3, p5, stiffness, true, height);
    n2.SetRenderer(Render);
    addRestriction(n2);

    n3 := TSpringRestriction.Create(P4, p5, stiffness, true, height);
    n3.SetRenderer(Render);
    addRestriction(n3);

    n4 := TSpringRestriction.Create(P4, p6, stiffness, true, height);
    n4.SetRenderer(Render);
    addRestriction(n4);

    n5 := TSpringRestriction.Create(p5, p6, stiffness, true, height);
    n5.SetRenderer(Render);
    addRestriction(n5);

//**********************
//    t1 := SpringRestriction.Create(p3, p4, stiffness, true, height);
//    t1.SetRenderer(Render);
//    addRestriction(t1);
//
//    t2 := SpringRestriction.Create(p2, p5, stiffness, true, height);
//    t2.SetRenderer(Render);
//    addRestriction(t2);

//***********************

//     n6 := SpringRestriction.Create(P6, P7, stiffness, true, height);
//     n6.SetRenderer(Render);
//     addRestriction(n6);

    m1 := TSolidCircle.Create(653, 101.5-7.8, radius, true);
    m1.Elasticity := 0;
    m1.mass := 1;
    // Ctr.x - rw / 2, Ctr.y - rh / 2
    m1.SetRenderer(Render);
    addParticle(m1);

    ms1 := TSpringRestriction.Create(P1, m1, stiffness, true, height);
    ms1.SetRenderer(Render);
    addRestriction(ms1);

    ms2 := TSpringRestriction.Create(P4, m1, stiffness, true, height);
    ms2.SetRenderer(Render);
    addRestriction(ms2);

//    t3 := SpringRestriction.Create(P2, m1, stiffness, true, height);
//    t3.SetRenderer(Render);
//    addRestriction(t3);

    // *************************************************
    P11 := TSolidCircle.Create(706-15-10, 60, radius, false);
    P11.mass := 1;
    P11.Elasticity := 0;
    P11.SetRenderer(Render);
    addParticle(P11);

    P21 := TSolidCircle.Create(706-15-10, 101.5, radius, true);
    P21.mass := 1;
    P21.Elasticity := 0;
    P21.SetRenderer(Render);
    addParticle(P21);

    P31 := TSolidCircle.Create(746.1-15-10, 101.5, radius, False); // temporal
    P31.mass := 1;
    P31.Elasticity := 0;
    P31.SetRenderer(Render);
    addParticle(P31);

    S11 := TSpringRestriction.Create(P11, P21, stiffness, true, height);
    S11.SetRenderer(Render);
    addRestriction(S11);

    S21 := TSpringRestriction.Create(P21, P31, stiffness, true, height);
    S21.SetRenderer(Render);
    addRestriction(S21);

    S31 := TSpringRestriction.Create(P31, P11, stiffness, true, height);
    S31.SetRenderer(Render);
    addRestriction(S31);

    P41 := TSolidCircle.Create(706-15-10, 139.3, radius, False);
    P41.Elasticity := 0;
    P41.SetRenderer(Render);
    addParticle(P41);

    p51 := TSolidCircle.Create(732.7-15-10, 139.3, radius, False); // temporal
    P51.Elasticity := 0;
    p51.SetRenderer(Render);
    addParticle(p51);

    p61 := TSolidCircle.Create(706-15-10, 188.9, radius, False, 1); // temporal
    P61.Elasticity := 0;
    //p61.PrintTrace := true;
    p61.SetRenderer(Render);
    addParticle(p61);

    n11 := TSpringRestriction.Create(P21, P41, stiffness, true, height);
    n11.SetRenderer(Render);
    addRestriction(n11);

    n21 := TSpringRestriction.Create(P31, p51, stiffness, true, height);
    n21.SetRenderer(Render);
    addRestriction(n21);

    n31 := TSpringRestriction.Create(P41, p51, stiffness, true, height);
    n31.SetRenderer(Render);
    addRestriction(n31);

    n41 := TSpringRestriction.Create(P41, p61, stiffness, true, height);
    n41.SetRenderer(Render);
    addRestriction(n41);

    n51 := TSpringRestriction.Create(p51, p61, stiffness, true, height);
    n51.SetRenderer(Render);
    addRestriction(n51);
//
////**********************
//    t4 := SpringRestriction.Create(p31, p41, stiffness, true, height);
//    t4.SetRenderer(Render);
//    addRestriction(t4);
//
//    t5 := SpringRestriction.Create(p21, p51, stiffness, true, height);
//    t5.SetRenderer(Render);
//    addRestriction(t5);

//***********************

    ms11 := TSpringRestriction.Create(P11, m1, stiffness, true, height);
    ms11.SetRenderer(Render);
    addRestriction(ms11);

    ms21 := TSpringRestriction.Create(P41, m1, stiffness, true, height);
    ms21.SetRenderer(Render);
    addRestriction(ms21);
//
//    t6 := SpringRestriction.Create(p21, m1, stiffness, true, height);
//    t6.SetRenderer(Render);
//    addRestriction(t6);

end;

procedure TJansenMechanism.Move(x: double);
    function GetRelativeAngle(Center, p: IVector): double;
    var
        delta: IVector;
    begin
        delta.SetPoint(p.x - Center.x, p.y - Center.y);
        result := ArcTan2(delta.y, delta.x);
    end;

var
    Center, pos: IVector;
    radius, angle: double;
begin
    // m1.px := m1.px + x;
    Center.SetPoint(638, 101.5-7.8);
    pos.SetPoint(m1.px, m1.py);
    radius := pos.Distance(Center);
    angle := GetRelativeAngle(Center, pos) + x;
    m1.px := (Cos(angle) * radius) + Center.x;
    m1.py := (Sin(angle) * radius) + Center.y;
end;

end.
