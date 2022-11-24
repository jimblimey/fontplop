unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, math, LCLIntf,
  LCLType, StdCtrls, StrUtils;

type
  TZXCharacter = Array[0..7] of Byte;
  { TfrmMain }
  TfrmMain = class(TForm)
    btnLoad: TButton;
    buttonPanel: TPanel;
    OpenDialog1: TOpenDialog;
    Panel3: TPanel;
    previewPanel: TScrollBox;
    procedure btnLoadClick(Sender: TObject);
    procedure buttonPanelResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    Characters: Array[32..127] of TZXCharacter;
    buttons: Array[0..7,0..7] of TShape;
    pixels: Array[0..7,0..7] of Byte;
    previews: Array[32..127] of TImage;
    CurrentCharacter: Integer;
    procedure SetButtonSize;
    procedure SetCharacter(c: Integer; d: TZXCharacter);
    procedure UpdateCharacter(c: Integer);
    procedure RedrawCharacter(c: Integer);
    procedure PreviewImageClick(Sender: TObject);
    procedure GetButtonPosition(const button: TShape; var x: Integer; var y: Integer);
  public

  end;

const
  Paper = clWhite;
  Ink = clBlack;
  GridSize = 8;

var
  frmMain: TfrmMain;
{$I specfont.inc}

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

function ColourContrast(incol: TColor): TColor;
const
  gamma = 2.2;
var
  r,g,b,L: Double;
begin
  r := Red(incol) / 255;
  g := Green(incol) / 255;
  b := Blue(incol) / 255;
  L := 0.2126 * power(R, gamma) + 0.7152 * power(G, gamma) + 0.0722 * power(B, gamma);
  if L > power(0.5, gamma) then Result := clBlack
  else Result := clWhite;
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
      buttons[x,y].OnMouseDown := @ButtonMouseDown;
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
    previews[c] := TImage.Create(Self);
    previews[c].Parent := previewPanel;
    previews[c].Width := 32;
    previews[c].Height := 32;
    previews[c].Left := x * 34;
    previews[c].Top := y * 34;
    previews[c].Stretch := true;
    previews[c].OnClick := @PreviewImageClick;
    inc(x);
    if x > 3 then
    begin
      inc(y);
      x := 0;
    end;
  end;
  CurrentCharacter := 32;
  For c := 32 to 127 do
  begin
    SetCharacter(c, ZXFont[c]);
    RedrawCharacter(c);
  end;
  SetCharacter(CurrentCharacter, ZXFont[CurrentCharacter]);
end;

procedure TfrmMain.buttonPanelResize(Sender: TObject);
begin
  SetButtonSize;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
var
  f: File of Byte;
  b: Byte;
  i,j: Integer;
  c: TZXCharacter;
begin
  if OpenDialog1.Execute then
  begin
    AssignFile(f,OpenDialog1.FileName);
    Reset(f);
    if Filesize(f) <> 768 then
    begin
      showmessage('Not a valid font!');
      CloseFile(f);
      exit;
    end;
    i := 32;
    while not eof(f) do
    begin
      for j := 0 to 7 do
      begin
        read(f,b);
        c[j] := b;
      end;
      SetCharacter(i,c);
      RedrawCharacter(i);
      inc(i);
    end;
    CloseFile(f);
  end;
end;

procedure TfrmMain.PreviewImageClick(Sender: TObject);
var
  i,t: Integer;
begin
  for i := 32 to 127 do
  begin
    if (Sender as TImage) = previews[i] then
    begin
      if i <> CurrentCharacter then
      begin
        t := CurrentCharacter;
        CurrentCharacter := i;
        SetCharacter(i,Characters[i]);
        RedrawCharacter(CurrentCharacter);
        RedrawCharacter(t);
      end;
    end;
  end;
end;

procedure TfrmMain.SetCharacter(c: Integer; d: TZXCharacter);
var
  i,j: Integer;
  s: String;
begin
  Characters[c] := d;
  for i := 0 to 7 do
  begin
    s := DecToBin(Characters[c][i]);
    for j := 1 to 8 do
    begin
      pixels[j-1,i] := StrToInt(s[j]);
      if s[j] = '1' then buttons[j-1,i].Brush.Color := clBlack
      else buttons[j-1,i].Brush.Color := clWhite;
    end;
  end;
end;

procedure TfrmMain.UpdateCharacter(c: Integer);
var
  i,j: Integer;
  s: String;
  b: Byte;
  d: TZXCharacter;
begin
  for i := 0 to 7 do
  begin
    s := '';
    for j := 0 to 7 do
    begin
      s := s + pixels[j,i].ToString;
    end;
    b := BinToDec(s);
    d[i] := b;
  end;
  Characters[c] := d;
end;

procedure TfrmMain.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cc: TColor;
  i,j: Integer;
  p: TPoint;
begin
  if Button = mbLeft then
  begin
    cc := (Sender as TShape).Brush.Color;
    if cc = Paper then
    begin
      (Sender as TShape).Brush.Color := Ink;
      GetButtonPosition(Sender as TShape,i,j);
      if (i > -1) and (j > -1) then pixels[i,j] := 1;
    end
    else
    begin
      (Sender as TShape).Brush.Color := Paper;
      GetButtonPosition(Sender as TShape,i,j);
      if (i > -1) and (j > -1) then pixels[i,j] := 0;
    end;
    UpdateCharacter(CurrentCharacter);
    RedrawCharacter(CurrentCharacter);
  end;
end;

procedure TfrmMain.GetButtonPosition(const button: TShape; var x: Integer; var y: Integer);
var
  i,j: Integer;
begin
  x := -1;
  y := -1;
  for i := 0 to 7 do
  begin
    for j := 0 to 7 do
    begin
      if button = buttons[i,j] then
      begin
        x := i;
        y := j;
      end;
    end;
  end;
end;

procedure TfrmMain.RedrawCharacter(c: Integer);
var
  bmp: graphics.TBitmap;
  i,j: Integer;
  s: String;
  p,fg,bg: TColor;
begin
  fg := clBtnText;
  bg := clBtnFace;
  bmp := graphics.TBitMap.Create;
  bmp.Width := 8;
  bmp.Height := 8;
  if c = CurrentCharacter then p := clHighlight
  else p := ColourContrast(fg);
  for i := 0 to 7 do
  begin
    s := DecToBin(Characters[c][i]);
    for j := 1 to 8 do
    begin
      if s[j] = '1' then bmp.Canvas.pixels[j-1,i] := fg
      else bmp.Canvas.pixels[j-1,i] := p;
    end;
  end;
  previews[c].Picture.Assign(bmp);
  bmp.Free;
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

