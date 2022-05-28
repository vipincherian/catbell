{

Copyright (C) 2021 Vipin Cherian

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
tsLicense as published by the Free Software Foundation; either
version 2 of the tsLicense, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public tsLicense for more details.

You should have received a copy of the GNU Library General Public
tsLicense along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

}

unit aboutform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, LCLIntf, LCLType, FileInfo;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    bbClose: TBitBtn;
    Image1: TImage;
    lblAppName: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lblLicense: TLabel;
    lsvBuild: TListView;
    Panel2: TPanel;
    ScrollBox1: TScrollBox;
    pgAdditional: TPageControl;
    Panel1: TPanel;
    sttUrl: TStaticText;
    tsLicense: TTabSheet;
    tsBuild: TTabSheet;
    procedure bbCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sttUrlClick(Sender: TObject);
  private
    { private declarations }
    //Buffer: Pointer;
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }


procedure TfrmAbout.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  Stream: TResourceStream;
  LicenseText: string;
  Buffer: Pointer;
  Item: TListItem;
  Info: TFileVersionInfo;
begin
  //memLicense.Text:='hello';
  // Read file version information and add
  Info := TFileVersionInfo.Create(Self);
  Info.ReadFileInfo;
  lblAppName.Caption := Info.VersionStrings.Values['ProductName'] +
    ' ' + Info.VersionStrings.Values['FileVersion'];
  Info.Free;

  Stream := TResourceStream.Create(hinstance, 'GPL2', RT_RCDATA);
  Buffer := AllocMem(Stream.Size + 1);
  Stream.ReadBuffer(Buffer^, Stream.Size);
  LicenseText := PAnsiChar(Buffer);
  lblLicense.Caption := LicenseText;
  Freemem(Buffer);
  //LicenseText := Stream.ReadAnsiString;
  Stream.Free;
  lsvBuild.BeginUpdate;

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Target OS';
  Item.SubItems.Add(
{$include %FPCTargetOS%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Target CPU';
  Item.SubItems.Add(
{$include %FPCTargetCPU%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Compiler(FPC) Version';
  Item.SubItems.Add(
{$include %FPCVersion%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Build date';
  Item.SubItems.Add(
{$include %DATE%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Build time';
  Item.SubItems.Add(
{$include %TIME%}
    );

  lsvBuild.EndUpdate;

  pgAdditional.ActivePage := tsLicense;

end;


procedure TfrmAbout.sttUrlClick(Sender: TObject);
begin
  OpenUrl(sttUrl.Caption);
end;

end.
