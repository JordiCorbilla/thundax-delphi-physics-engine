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
unit tdpe.lib.richedit.writer;

interface

uses
  tdpe.lib.writer.contract, ComCtrls, Graphics, tdpe.lib.logging;

type
  TRichEditWriter = class(TInterfacedObject, IWriter)
  private
    FRichEdit: TRichEdit;
  public
    constructor Create(richedit: TRichEdit);
    procedure AddText(s: string; const colour: TColor = clBlack);
  end;

implementation

uses
  Messages, Windows;

{ TRichEditWriter }

procedure TRichEditWriter.AddText(s: string; const colour: TColor);
begin
  Exit;
  Self.FRichEdit.SelAttributes.Color := colour;
  Self.FRichEdit.SelText := s;
  Self.FRichEdit.SelAttributes.Color := clWindowText;
  Self.FRichEdit.SelText := sLineBreak;
  Self.FRichEdit.Perform(EM_SCROLL, SB_LINEDOWN, 0);
end;

constructor TRichEditWriter.Create(richedit: TRichEdit);
begin
  Self.FRichEdit := richedit;
end;

end.
