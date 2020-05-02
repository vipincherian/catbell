{

Copyright (C) 2019 Vipin Cherian

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

}

unit aboutform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    bbClose: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ScrollBox1: TScrollBox;
    StaticText1: TStaticText;
    Technical: TPageControl;
    Panel1: TPanel;
    License: TTabSheet;
    TabSheet2: TTabSheet;
    procedure bbCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure StaticText3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.Image1Click(Sender: TObject);
begin

end;

procedure TfrmAbout.Label4Click(Sender: TObject);
begin

end;

procedure TfrmAbout.FormDestroy(Sender: TObject);
begin

end;

procedure TfrmAbout.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.StaticText1Click(Sender: TObject);
begin

end;

procedure TfrmAbout.StaticText3Click(Sender: TObject);
begin

end;

end.

