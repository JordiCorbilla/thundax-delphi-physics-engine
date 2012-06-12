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
unit tdpe.lib.engine.wrapper;

interface

uses
  tdpe.lib.engine, tdpe.lib.force.contract, tdpe.lib.particle.group;

type
  TFluentEngine = class(TEngine)
  public
    function AddInitialForce(const vector: IForce): TFluentEngine;
    function AddDamping(const Damping: Double): TFluentEngine;
    function AddRestrictionCollitionCycles(const RestrictionCollisionCycles: Integer): TFluentEngine;
    function AddGroups(group: TGroup): TFluentEngine;
    class function New(const deltaTime: Double = 0.25): TFluentEngine;
  end;

implementation

{ TFluentEngine }

function TFluentEngine.AddInitialForce(const vector: IForce): TFluentEngine;
begin
  Self.AddForce(vector);
  result := Self;
end;

function TFluentEngine.AddGroups(group: TGroup): TFluentEngine;
begin
  Self.AddGroup(group);
  result := Self;
end;

class function TFluentEngine.New(const deltaTime: Double): TFluentEngine;
begin
  result := Create(deltaTime);
end;

function TFluentEngine.AddDamping(const Damping: Double): TFluentEngine;
begin
  Self.Damping := Damping;
  result := Self;
end;

function TFluentEngine.AddRestrictionCollitionCycles(const RestrictionCollisionCycles: Integer): TFluentEngine;
begin
  Self.RestrictionCollisionCycles := RestrictionCollisionCycles;
  result := Self;
end;

end.
