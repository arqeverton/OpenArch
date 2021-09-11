{---------------------------------------------------------------------------------
This file is part of OpenARCK Project.

   Copyright (C) 2021  Everton Teles
   email: info@evertonteles.com
   home: www.evertonteles.com

OpenArch is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
----------------------------------------------------------------------------------

// History:
2021 08 28 - ET - Unit implementation Start's.
}

unit OpenArchUIManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Controls,
  //OpenArch:
  OpenArchProject;
type

  { TArchUIManagement }

  TArchUIManagement = class
    private
    public
      ArchProject:TArchProject;
      Parent: TComponent;
      constructor Create(AOwner: TComponent;AFilePath:String);
      procedure RefreshStoryEnable;
  end;

implementation
uses
  OpenArchMainForm, OpenArchDesignFrame, OpenArchNavigatorFrame, OpenArchToolBoxFrame,
  OpenArchMainToolbar, OpenArchObjectInfoFrame;

{ TArchUIManagement }

constructor TArchUIManagement.Create(AOwner: TComponent; AFilePath:String);
var
  vMForm: TMainForm;
begin
  ArchProject:= TArchProject.Create(AOwner,AFilePath);
  Parent:= AOwner;
  vMForm:= Parent as TMainForm;
  with vMForm do
  begin
    //vMForm.ArchProject:= Self.ArchProject;
    ObjectInfo:= TArchObjectInfoFrame.Create(vmForm,Self);
    MainToolBar:= TArchMainToolBarFrame.Create(vMForm);
    ToolBox:= TArchToolBoxFrame.Create(vmForm);
    Navigator:= TArchNavigatorFrame.Create(vMForm, Self);
    ArchDesign:= TArchDesignFrame.Create(vMForm, Self);
  end;
end;

procedure TArchUIManagement.RefreshStoryEnable;
var
  vMForm: TMainForm;
begin
  vMForm:= Parent as TMainForm;
  with vMForm do
  begin
    ArchDesign.RefreshStoryEnable;
  end;
end;

end.

