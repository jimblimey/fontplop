unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, math, LCLIntf,
  LCLType, StrUtils;

type
  TZXCharacter = Array[0..7] of Byte;
  { TfrmMain }
  TfrmMain = class(TForm)
    buttonPanel: TPanel;
    Panel3: TPanel;
    previewPanel: TScrollBox;
    procedure buttonPanelResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Characters: Array[32..127] of TZXCharacter;
    buttons: Array[0..7,0..7] of TShape;
    pixels: Array[0..7,0..7] of Byte;
    previews: Array[32..127] of TShape;
    CurrentCharacter: Integer;
    procedure SetButtonSize;
  public

  end;

const
  Paper = clWhite;
  Ink = clBlack;
  GridSize = 8;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

function Pow(i, k: Integer): Integer;
var
  j, Count: Integer;
begin
  if k>0 then j:=2
    else j:=1;
  for Count:=1 to k-1 do
    j:=j*2;
  Result:=j;
end;

function BinToDec(Str: string): Integer;
var
  Len, Res, i: Integer;
  Error: Boolean;
begin
  Error:=False;
  Len:=Length(Str);
  Res:=0;
  for i:=1 to Len do
    if (Str[i]='0')or(Str[i]='1') then
      Res:=Res+Pow(2, Len-i)*StrToInt(Str[i])
    else
    begin
      Error:=True;
      Break;
    end;
  if Error=True then Result:=0
    else Result:=Res;
end;

function DecToBin(Value: Byte): string;
var
  i: Integer;
begin
  SetLength(Result, 8);
  for i := 1 to 8 do
  begin
    if (Value shr (8-i)) and 1 = 0 then
    begin
      Result[i] := '0'
    end
    else
    begin
      Result[i] := '1';
    end;
  end;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  c,x,y,n,h,w: Integer;
begin
  c := 0;
  for x := 0 to 7 do
  begin
    for y := 0 to 7 do
    begin
      buttons[x,y] := TShape.Create(Self);
      buttons[x,y].Parent := buttonPanel;
      //buttons[x,y].OnMouseDown := @ButtonMouseDown;
      buttons[x,y].Brush.Color := Paper;
      buttons[x,y].Pen.Color := Paper;
      pixels[x,y] := 0;
      inc(c);
    end;
  end;
  SetButtonSize;
  x := 0;
  y := 0;
  //h := GetSystemMetrics(SM_CYHSCROLL);
  w := GetSystemMetrics(SM_CXVSCROLL);
  previewPanel.Width := 134+w+6;
  for c := 32 to 127 do
  begin
    previews[c] := TShape.Create(Self);
    previews[c].Parent := previewPanel;
    previews[c].Width := 32;
    previews[c].Height := 32;
    previews[c].Brush.Color := Paper;
    previews[c].Pen.Color := Paper;
    previews[c].Left := x * 34;
    previews[c].Top := y * 34;
    inc(x);
    if x > 3 then
    begin
      inc(y);
      x := 0;
    end;
  end;
  CurrentCharacter := 32;
end;

procedure TfrmMain.buttonPanelResize(Sender: TObject);
begin
  SetButtonSize;
end;

procedure TfrmMain.SetButtonSize;
var
  x,y,n,h,c: Integer;
begin
  h := min(buttonPanel.Width,buttonPanel.Height);
  c := GridSize;
  n := (h div c)-2;
  for x := 0 to GridSize-1 do
  begin
    for y := 0 to GridSize-1 do
    begin
      buttons[x,y].Width := n;
      buttons[x,y].Height := n;
      buttons[x,y].Left := x * (n + 2);
      buttons[x,y].Top := y * (n + 2);
    end;
  end;
end;

end.

