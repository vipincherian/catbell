unit adjustform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DateTimePicker;

type

  { TfrmTimerAdjust }

  TfrmTimerAdjust = class(TForm)
    bbApply: TBitBtn;
    cmbOptions: TComboBox;
    dtpAdjust: TDateTimePicker;
    procedure cmbOptionsChange(Sender: TObject);
  private

  public

  end;

var
  frmTimerAdjust: TfrmTimerAdjust;
const
  ADJUST_SHORTEN = 0;
  ADJUST_EXTEND = 1;
  ADJUST_STOPBY = 2;

implementation

{$R *.lfm}

{ TfrmTimerAdjust }

procedure TfrmTimerAdjust.cmbOptionsChange(Sender: TObject);
begin
  case cmbOptions.ItemIndex of
    ADJUST_SHORTEN:
      dtpAdjust.Kind := dtkDate;
    ADJUST_EXTEND:
      dtpAdjust.Kind := dtkDate;
    ADJUST_STOPBY:
      dtpAdjust.Kind := dtkDateTime;
  end;
end;

end.

