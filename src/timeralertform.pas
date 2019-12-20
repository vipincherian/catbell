unit timeralertform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, Buttons, LazLogger;

type

  { TfrmTimerAlert }

  TfrmTimerAlert = class(TForm)
    bbClose: TBitBtn;
    Label1: TLabel;
    lbMessages: TListBox;
    FTimer: TTimer;
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { public declarations }
  end;

var
  frmTimerAlert: TfrmTimerAlert;

implementation

{$R *.lfm}

{ TfrmTimerAlert }

procedure TfrmTimerAlert.FormCreate(Sender: TObject);
begin
  //BorderStyle := bsNone;
  //OnShow := @FormShow;
  //AlphaBlend:=True;
  AlphaBlendValue:=50;
end;

procedure TfrmTimerAlert.FormShow(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  //FTimer.Interval:=2000;
end;

procedure TfrmTimerAlert.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_THICKFRAME;
end;

procedure TfrmTimerAlert.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTimerAlert.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  //DebugLn('Entering FormClose');
  lbMessages.Items.Clear;
  //DebugLn('Exiting FormClose');
end;

end.

