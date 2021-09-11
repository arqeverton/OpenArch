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
 2021 04 20 - ET - Unit implementation Start's.
}

unit OpenArchGLSceneTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, GLScene, GLViewer,
  GLGeomObjects, GLObjects;

type

  { TFormTestes }

  TFormTestes = class(TForm)
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLLines1: TGLLines;
    GLLines2: TGLLines;
    GLMemoryViewer1: TGLMemoryViewer;
    GLPolygon1: TGLPolygon;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
  private

  public

  end;

var
  FormTestes: TFormTestes;

implementation

{$R *.frm}

end.

