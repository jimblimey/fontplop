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
  private
    Characters: Array[32..127] of TZXCharacter;
    buttons: Array[0..7,0..7] of TShape;
    pixels: Array[0..7,0..7] of Byte;
    previews: Array[32..127] of TImage;
    CurrentCharacter: Integer;
    procedure SetButtonSize;
    procedure SetCharacter(c: Integer; d: TZXCharacter);
    procedure RedrawCharacter(c: Integer);
    procedure PreviewImageClick(Sender: TObject);
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
      if s[j] = '1' then buttons[j-1,i].Brush.Color := clBlack
      else buttons[j-1,i].Brush.Color := clWhite;
    end;
  end;
end;

procedure TfrmMain.RedrawCharacter(c: Integer);
var
  bmp: graphics.TBitmap;
  i,j: Integer;
  s: String;
  p: TColor;
begin
  bmp := graphics.TBitMap.Create;
  bmp.Width := 8;
  bmp.Height := 8;
  if c = CurrentCharacter then p := RGB(255,0,255)
  else p := clWhite;
  for i := 0 to 7 do
  begin
    s := DecToBin(Characters[c][i]);
    for j := 1 to 8 do
    begin
      if s[j] = '1' then bmp.Canvas.pixels[j-1,i] := clBlack
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

