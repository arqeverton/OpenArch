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
 2020 02 03 - ET - Unit implementation Start's.
}

unit OpenArchDraw;

{$mode objfpc}{$H+}

interface

uses
  { OpenGl: }
  GL, glu, GLext,

  { GLScene: }
  GLScene,
  GLVectorGeometry,
  GLVectorTypes,
  GLTexture,
  GLVectorLists,

  { OpenARCK: }
  // Units:
  OpenArchVectorGeometry;

type

  { TArchDraw }
  TArchDraw = class
    private
    public
      constructor Create();
      procedure Point(const APoint: TVector3f);
      procedure Line(const AStartPoint, AEndPoint: TVector3f; AColor: TVector4f;
        APattern: TGLushort; AWidth: Single);
      procedure Arc(const AStartPoint, AEndPoint, ACenterPoint: TVector3f;
        AColor: TVector4f);
      procedure PolygonFill(const AVertices: TArchVectorArray; AColor: TVector4f);
      procedure PolygonContour(const AVertices: TArchVectorArray; AColor: TVector4f;
        APattern: TGLushort; AWidth: Single);
      procedure Face(APolygonArray: TPolygonArray);
  end;


implementation

uses

{ FreePascal: }
Dialogs,
SysUtils;

{ TArchDraw }
constructor TArchDraw.Create();
begin

end;

procedure TArchDraw.Point(const APoint: TVector3f);
begin

glEnable(GL_POINT_SMOOTH);
glEnable(GL_BLEND);
glPointSize(6);
  glBegin(GL_POINTS);
  glColor4f(0,0,0,1);
  glVertex3f(APoint.X,
             APoint.Y,
             APoint.Z);
glEnd;
end;

procedure TArchDraw.Line(const AStartPoint, AEndPoint: TVector3f;
AColor: TVector4f; APattern: TGLushort; AWidth: Single);
begin
glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
glEnable(GL_LINE_SMOOTH);
glEnable(GL_BLEND);
glLineWidth(AWidth);
glEnable(GL_LINE_STIPPLE);
glLineStipple(1,APattern);
glBegin(GL_LINES);
  glColor4f(AColor.X,
            AColor.Y,
            AColor.Z,
            AColor.W);
  glVertex3f(AStartPoint.X,
             AStartPoint.Y,
             AStartPoint.Z);
  glVertex3f(AEndPoint.X,
             AEndPoint.Y,
             AEndPoint.Z);
glEnd;
end;

procedure TArchDraw.Arc(const AStartPoint, AEndPoint, ACenterPoint: TVector3f;
AColor: TVector4f);
begin

end;

procedure TArchDraw.PolygonFill(const AVertices: TArchVectorArray; AColor:TVector4f);
begin
glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
glEnable(GL_BLEND);
GLEnableClientState(GL_VERTEX_ARRAY);
  GLVertexPointer(3,GL_FLOAT,0,@AVertices[0]);
  //Draw Rectangle fill
  GlColor4f((AColor.X),
            (AColor.Y),
            (AColor.Z),
            (AColor.W));
  GLDrawArrays(GL_TRIANGLE_FAN, 0,Length(AVertices));
GLDisableClientState(GL_VERTEX_ARRAY);
end;

procedure TArchDraw.PolygonContour(const AVertices: TArchVectorArray;
AColor: TVector4f; APattern: TGLushort; AWidth: Single);
begin
glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
glEnable(GL_BLEND);
GLEnableClientState(GL_VERTEX_ARRAY);
  GLVertexPointer(3,GL_FLOAT,0,@AVertices[0]);
  //Draw Rectangle Pen
  GLLineWidth(AWidth);
  GlColor4f((AColor.X),
            (AColor.Y),
            (AColor.Z),
            (AColor.W));
  GLDrawArrays(GL_LINE_LOOP, 0,Length(AVertices));
GLDisableClientState(GL_VERTEX_ARRAY);
end;

procedure TArchDraw.Face(APolygonArray: TPolygonArray);
function IsClockwise(AVectorArray: TArchVectorArray):boolean;
  var
    i:integer;
    VctAff: Array of TAffineVector;
begin
  SetLength(VctAff, Length(AVectorArray));
  for i:=0 to Length(VctAff)-1 do
    begin
      VctAff[i].X:= AVectorArray [i].X;
      VctAff[i].Y:= AVectorArray[i].Y;
      VctAff[i].Z:= AVectorArray[i].Z;
    end;
  if PolygonSignedArea(@VctAff, Length(VctAff)) < 0 then
    Result:= false
  else
    Result:= true;
end;
var
i,j : integer;
Poly: PGLUtesselator;
begin
Poly:= gluNewTess();
gluTessCallback(Poly, GLU_TESS_BEGIN, TCallback(@glBegin));
gluTessCallback(Poly, GLU_TESS_VERTEX, TCallback(@glVertex3dv));
gluTessCallback(Poly, GLU_TESS_END, TCallback(@glEnd));
glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
glEnable(GL_CULL_FACE);
gluTessBeginPolygon( Poly, nil);
{ The first array of vertices is
the contourn of the face, the
others arrays are the holes that
have th face}
  if not (Length(APolygonArray)=0) then
    begin
      for i:=0 to Length(APolygonArray)-1 do
        begin
          gluTessBeginContour(Poly);
          glColor3f(0,0,0);
          if IsClockwise(APolygonArray[i]) then
            begin
              for j:= 0 to Length(APolygonArray[i]) -1 do
                begin
                  //glVertex3dv(PGLdouble(@APolygonArray[i][j]));
                  gluTessVertex(Poly, PGLdouble(@APolygonArray[i][j]), @APolygonArray[i][j]);
                end;
            end else
            begin
              for  j:=Length(APolygonArray[i])-1 downto 0 do
                begin
                  //glVertex3dv(PGLdouble(@APolygonArray[i][j]));
                  gluTessVertex(Poly, PGLdouble(@APolygonArray[i][j]), @APolygonArray[i][j]);
                end;
            end;
          gluTessEndContour(poly);
        end;
    end;
gluTessEndPolygon(Poly);
ShowMessage('TESSELATION');
gluDeleteTess(poly);
end;
end.

