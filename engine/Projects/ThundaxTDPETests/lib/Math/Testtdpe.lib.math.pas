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
unit Testtdpe.lib.math;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, Types, math, tdpe.lib.math;

type
  // Test methods for class TMathHelper

  TestTMathHelper = class(TTestCase)
  strict private
    FMathHelper: TMathHelper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCompare;
  end;

implementation

procedure TestTMathHelper.SetUp;
begin
  FMathHelper := TMathHelper.Create;
end;

procedure TestTMathHelper.TearDown;
begin
  FMathHelper.Free;
  FMathHelper := nil;
end;

procedure TestTMathHelper.TestCompare;
var
  ReturnValue: Boolean;
  MethodComp: string;
  Value2: Double;
  Value1: Double;
begin
  Value1 := 10;
  Value2 := 10;
  MethodComp := '=';
  ReturnValue := FMathHelper.Compare(Value1, Value2, MethodComp);
  CheckTrue(ReturnValue, 'Wrong value, expected true but was false');

  Value1 := 10;
  Value2 := 12;
  MethodComp := '<';
  ReturnValue := FMathHelper.Compare(Value1, Value2, MethodComp);
  CheckTrue(ReturnValue, 'Wrong value, expected true but was false');

  Value1 := 12;
  Value2 := 10;
  MethodComp := '>';
  ReturnValue := FMathHelper.Compare(Value1, Value2, MethodComp);
  CheckTrue(ReturnValue, 'Wrong value, expected true but was false');

  Value1 := 10;
  Value2 := 12;
  MethodComp := '<=';
  ReturnValue := FMathHelper.Compare(Value1, Value2, MethodComp);
  CheckTrue(ReturnValue, 'Wrong value, expected true but was false');

  Value1 := 12;
  Value2 := 10;
  MethodComp := '>=';
  ReturnValue := FMathHelper.Compare(Value1, Value2, MethodComp);
  CheckTrue(ReturnValue, 'Wrong value, expected true but was false');

  Value1 := 12;
  Value2 := 7;
  MethodComp := '<>';
  ReturnValue := FMathHelper.Compare(Value1, Value2, MethodComp);
  CheckTrue(ReturnValue, 'Wrong value, expected true but was false');
end;

initialization

RegisterTest(TestTMathHelper.Suite);

end.