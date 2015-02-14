{-----------------------------------------------------------------------------
 Unit Name: ExportMesh
 Author:    oranke
 Date:      2015-02-12
 Purpose:
 History:
-----------------------------------------------------------------------------}

unit ExportMesh;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,

  Voxel,
  Voxel_Engine,
  VectorUtil;

const
  CUBIC_NORMALS : packed array [0..6-1] of TVector3sb = (
    // Front face
    ( 0, 0, 1 ),
    // Back Face
    ( 0, 0,-1 ),
    // Top Face
    ( 0, 1, 0 ),
    // Bottom Face
    ( 0,-1, 0 ),
    // Right face
    ( 1, 0, 0 ),
    // Left Face
    (-1, 0, 0 )
  );

type
  PVxVertex = ^TVxVertex;
  TVxVertex = record
    DupIndex: I32;
    case Integer of
      0: (x, y, z: I32);
      1: (Arr: array [0..2] of I32);
  end;

  PVxFace = ^TVxFace;
  TVxFace = record
    c, n: I32;
    case Integer of
      0: (v0, v1, v2: I32);
      1: (Arr: array [0..2] of I32);
  end;


  TObjects = class (TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TRecords = class (TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

// 복셀 -> Monotone.
// http://0fps.net/2012/07/07/meshing-minecraft-part-2/
// https://github.com/mikolalysenko/mikolalysenko.github.com/blob/master/MinecraftMeshes2/js/monotone.js
procedure BuildMesh(aVertices, aFaces: TRecords; aEraseDupVt: Boolean);


procedure ExportToObjFile(const aFileName: String);
function GetCubicNormalIndex(const aNormal: TVector3f): I32;


implementation

uses
  ogl3dview_engine, 
  pngimage;

{ TObjects }

procedure TObjects.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then TObject(Ptr).Free;
end;

{ TRecords }

procedure TRecords.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then Dispose(Ptr);
end;

// 복셀 -> Monotone 테스트.
// http://0fps.net/2012/07/07/meshing-minecraft-part-2/
// https://github.com/mikolalysenko/mikolalysenko.github.com/blob/master/MinecraftMeshes2/js/monotone.js

type
  TMeshSide = array[0..1] of I32;

  TMonotoneMesh = class
    Color: I32;
    Left: array of TMeshSide;
    Right : array of TMeshSide;

    constructor Create(c, v, ul, ur: I32);
    procedure close_off(v: I32);
    procedure merge_run(v, u_l, u_r: I32);
  end;

{ TMonotoneMesh }

constructor TMonotoneMesh.Create(c, v, ul, ur: I32);
begin
  Color := c;
  SetLength(Left, 1);
  Left[0][0] := ul;
  Left[0][1] := v;

  SetLength(Right, 1);
  Right[0][0] := ur;
  Right[0][1] := v;
end;

procedure TMonotoneMesh.close_off(v: I32);
var
  cv: I32;
begin
  //lmsd := Left[Length(Left)-1];
  //rmsd := Right[Length(Right)-1];

  cv := Left[Length(Left)-1][0];
  SetLength(Left, Length(Left)+1);
  Left[Length(Left)-1][0] := cv;
  Left[Length(Left)-1][1] := v;

  cv := Right[Length(Right)-1][0];
  SetLength(Right, Length(Right)+1);
  Right[Length(Right)-1][0] := cv;
  Right[Length(Right)-1][1] := v;

end;

procedure TMonotoneMesh.merge_run(v, u_l, u_r: I32);
var
  cv: I32;
begin
  cv := Left[Length(Left)-1][0];
  if cv <> u_l then
  begin
    SetLength(Left, Length(Left)+2);
    Left[Length(Left)-2][0] := cv;
    Left[Length(Left)-2][1] := v;

    Left[Length(Left)-1][0] := u_l;
    Left[Length(Left)-1][1] := v;
  end;

  cv := Right[Length(Right)-1][0];
  if cv <> u_r then
  begin
    SetLength(Right, Length(Right)+2);
    Right[Length(Right)-2][0] := cv;
    Right[Length(Right)-2][1] := v;

    Right[Length(Right)-1][0] := u_r;
    Right[Length(Right)-1][1] := v;
  end;
end;

function GetCubicNormalIndex(const aNormal: TVector3f): I32;
begin
  if aNormal[C_Z] > 0 then Result := 0 else
  if aNormal[C_Z] < 0 then Result := 1 else

  if aNormal[C_Y] > 0 then Result := 2 else
  if aNormal[C_Y] < 0 then Result := 3 else

  if aNormal[C_X] > 0 then Result := 4 else
  if aNormal[C_X] < 0 then Result := 5 else

  Result := -1;
end;


procedure BuildMesh(aVertices, aFaces: TRecords; aEraseDupVt: Boolean);
var
  dims: array [0..2] of U8;

  d,
  i, j, k: I32;
  u, v: I32;
  x, q: array [0..2] of I32;
  runs,
  frontier,
  next_frontier,
  left_index,
  right_index,
  stack,
  tmp: array of I32;
  delta: array [0..1, 0..1] of I32;

  n, nf: I32;
  nr, p, c: I32;

  a, b: I32;
  fp: I32;

  vxl: TVoxelUnpacked;

  Polygons: TObjects;

  mp, np: TMonotoneMesh;
  p_l, p_r, p_c, r_l, r_r, r_c: I32;
  flipped: Boolean;

  z: TMeshSide; 

  vp: PVxVertex;

  bottom, top, l_i, r_i: I32;
  side: Boolean;

  n_side: Boolean;
  l, r: TMeshSide;
  idx: I32;
  vert: TMeshSide;
  det: I32;
  facep: PVxFace;

  //VoxelColor: Voxel_Engine.TVector3f;
  vps: array [0..2] of PVxVertex;
  v0, v1: VectorUtil.TVector3f;
  Normal: VectorUtil.TVector3f;
  
begin
  Polygons:= TObjects.Create;

  with ActiveSection.Tailer do
  begin
    dims[0] := XSize;
    dims[1] := YSize;
    dims[2] := ZSize;
  end;


  d := 0;
  while d < 3 do
  begin
    u := (d+1) mod 3;  //u and v are orthogonal directions to d
    v := (d+2) mod 3;

    FillChar(x, SizeOf(I32)*3, #0);
    FillChar(q, SizeOf(I32)*3, #0);

    SetLength(runs, 2 * (dims[u]+1) );
    //FillChar(runs, 2 * (dims[u]+1) * SizeOf(I32), #0);
    SetLength(frontier, dims[u]);
    //FillChar(frontier, dims[u] * SizeOf(I32), #0);
    SetLength(next_frontier, dims[u]);
    //FillChar(next_frontier, dims[u] * sizeOf(I32), #0);
    SetLength(left_index, 2 * dims[v]);
    //FillChar(left_index, 2 * dims[v] * SizeOf(I32), #0);
    SetLength(right_index, 2 * dims[v]);
    //FillChar(right_index, 2 * dims[v] * SizeOf(I32), #0);
    SetLength(stack, 24 * dims[v]);
    //FillChar(stack, 24 * dims[v] * SizeOf(I32), #0);

    //FillChar(delta, SizeOf(I32) * 2 * 2, #0);

    //q points along d-direction
    q[d] := 1;

    //Initialize sentinel
    x[d] := -1;
    while x[d] < dims[d] do
    begin
      // --- Perform monotone polygon subdivision ---
      n := 0;
      nf := 0;

      Polygons.clear;

      x[v] := 0;
      while x[v] < dims[v] do
      begin
        //Make one pass over the u-scan line of the volume to run-length encode polygon
        nr := 0;
        // js 예제에서는 0이 빈 곳을 나타냄. 여기서는 사용되지 않는 복셀은 -1로.
        // -->> 0 그대로 쓰고 칼라값에 1을 더해 쓰자.
        p := 0; //c := 0;

        x[u] := 0;
        while x[u] < dims[u] do
        begin
          //p := c;
          
          //Compute the type for this face
          a := 0;
          if 0 <= x[d] then
          begin
            ActiveSection.GetVoxel(x[0], x[1], x[2], vxl);
            if vxl.Used then
              a := vxl.Colour +1
          end;

          b := 0;
          if x[d] < dims[d]-1 then
          begin
            ActiveSection.GetVoxel(x[0]+q[0], x[1]+q[1], x[2]+q[2], vxl);
            if vxl.Used then
              b := vxl.Colour +1
          end;

          c := a;
          //!! Check!
          if Boolean(a) = Boolean(b) then
            c := 0
          else if not Boolean(a) then
            c := -b;

          //If cell type doesn't match, start a new run
          if p <> c then
          begin
            runs[nr] := x[u];
            inc(nr);
            runs[nr] := c;
            inc(nr);
          end;

          Inc(x[u]);

          p := c;
        end;
        //p := c;

        //Add sentinel run
        runs[nr] := dims[u];
        inc(nr);
        runs[nr] := 0;
        inc(nr);
        
        //Update frontier by merging runs
        fp := 0;
        i :=0; j := 0;
        while (i < nf) and (j < nr-2) do
        begin
          if (frontier[i] >= 0) and (frontier[i] < Polygons.Count) then
          begin
            mp := Polygons[frontier[i]];
            p_l := mp.Left[Length(mp.Left)-1][0];
            p_r := mp.Right[Length(mp.Right)-1][0];
            p_c := mp.Color; 
          end else
          begin
            mp := nil;
            p_l := 0;
            p_r := 0;
            p_c := 0;
          end;

          r_l := runs[j];   // Start of run
          r_r := runs[j+2]; // End of run
          r_c := runs[j+1]; // Color of run

          //Check if we can merge run with polygon
          if (r_r > p_l) and (p_r > r_l) and (r_c = p_c) then
          begin
            //Merge run
            if Assigned(mp) then
              mp.merge_run(x[v], r_l, r_r);
              
            //Insert polygon into frontier
            next_frontier[fp] := frontier[i];
            Inc(fp);

            Inc(i);
            Inc(j, 2);
          end else
          begin
            //Check if we need to advance the run pointer
            if (r_r <= p_r) then
            begin
              //if r_c <> 0 then //(!!r_c) {
              if Boolean(r_c) then
              begin
                np := TMonotoneMesh.Create(r_c, x[v], r_l, r_r);
                //WriteLn('MonotoneMesh Created!');
                next_frontier[fp] := Polygons.Count;
                Inc(fp);
                Polygons.Add(np);
              end;
              Inc(j, 2);
            end;

            //Check if we need to advance the frontier pointer
            if(p_r <= r_r) then
            begin
              if Assigned(mp) then
                mp.close_off(x[v]);

              Inc(i);
              //WriteLn('Inc i-2. ', i);
            end;
          end;

          //WriteLn(i, ', ', j);
        end;

        //Close off any residual polygons
        while i < nf do
        begin
          TMonotoneMesh(Polygons[frontier[i]]).close_off(x[v]);
          Inc(i);
        end;

        //Add any extra runs to frontier
        while j<nr-2 do
        begin
          r_l  := runs[j];
          r_r  := runs[j+2];
          r_c  := runs[j+1];

          //if r_c <> 0 then //(!!r_c) {
          if Boolean(r_c) then
          begin
            np := TMonotoneMesh.Create(r_c, x[v], r_l, r_r);
            //WriteLn('MonotoneMesh Created2!');
            next_frontier[fp] := Polygons.Count;
            Inc(fp);
            Polygons.Add(np);
          end;
          Inc(j, 2);
        end;

        //Swap frontiers
        tmp := next_frontier;
        next_frontier := frontier;
        frontier := tmp;
        nf := fp;

        inc(x[v]);
      end;

      //Close off frontier
      for i := 0 to nf-1 do
        TMonotoneMesh(Polygons[frontier[i]]).close_off(dims[v]);

      //WriteLn('Polygons ', Polygons.Count, ', ', x[d], ', ', dims[d]);

      // --- Monotone subdivision of polygon is complete at this point ---

      Inc(x[d]);

      //Now we just need to triangulate each monotone polygon
      for i := 0 to Polygons.Count - 1 do
      begin
        mp := Polygons[i];
        c := mp.Color;
        flipped := false;

        if c < 0 then
        begin
          flipped := true;
          c := -c;
        end;

        for j := 0 to Length(mp.Left) - 1 do
        begin
          left_index[j] := aVertices.Count;
          z := mp.Left[j];

          new(vp);
          vp^.Arr[d] := x[d];
          vp^.Arr[u] := z[0];
          vp^.Arr[v] := z[1];
          vp^.DupIndex := -1;

          aVertices.Add(vp);
        end;

        for j := 0 to Length(mp.Right) - 1 do
        begin
          right_index[j] := aVertices.Count;
          z := mp.Right[j];

          new(vp);
          vp^.Arr[d] := x[d];
          vp^.Arr[u] := z[0];
          vp^.Arr[v] := z[1];
          vp^.DupIndex := -1; 

          aVertices.Add(vp);
        end;

        //Triangulate the monotone polygon
        bottom := 0;
        top := 0;
        l_i := 1;
        r_i := 1;
        side := true;  //true = right, false = left

        stack[top] := left_index[0];
        Inc(top);
        stack[top] := mp.left[0][0];
        Inc(top);
        stack[top] := mp.left[0][1];
        Inc(top);

        stack[top] := right_index[0];
        Inc(top);
        stack[top] := mp.right[0][0];
        Inc(top);
        stack[top] := mp.right[0][1];
        Inc(top);

        while (l_i < Length(mp.left)) or (r_i < Length(mp.right)) do
        begin
          n_side := false;

          if (l_i = Length(mp.left)) then
            n_side := true
          else if (r_i <> Length(mp.Right)) then
          begin
            l := mp.left[l_i];
            r := mp.right[r_i];
            n_side := l[1] > r[1];
          end;

          if n_side then
          begin
            idx := right_index[r_i];
            vert := mp.Right[r_i];
          end else
          begin
            idx := left_index[l_i];
            vert := mp.Left[l_i];
          end;

          if (n_side <> side) then
          begin
            //Opposite side
            while bottom+3 < top do
            begin
              New(facep);

              if(flipped = n_side) then
              begin
                facep^.v0 := stack[bottom];
                facep^.v1 := stack[bottom+3];
                facep^.v2 := idx;
                facep^.c := c-1;
                //faces.push([ stack[bottom], stack[bottom+3], idx, c]);
              end else
              begin
                facep^.v0 := stack[bottom+3];
                facep^.v1 := stack[bottom];
                facep^.v2 := idx;
                facep^.c := c-1;
                //faces.push([ stack[bottom+3], stack[bottom], idx, c]);
              end;

              aFaces.Add(facep);

              Inc(bottom, 3);
            end;

          end else
          begin
            //Same side
            while bottom+3 < top do
            begin
              for j := 0 to 2 - 1 do
              for k := 0 to 2 - 1 do
                delta[j][k] := stack[top-3*(j+1)+k+1] - vert[k];

              det := delta[0][0] * delta[1][1] - delta[1][0] * delta[0][1];
              if n_side = (det > 0) then Break;

              if (det <> 0) then
              begin
                New(facep);

                if (flipped = n_side) then
                begin
                  facep^.v0 := stack[top-3];
                  facep^.v1 := stack[top-6];
                  facep^.v2 := idx;
                  facep^.c := c-1;
                  //faces.push([ stack[top-3], stack[top-6], idx, c ]);
                end else
                begin
                  facep^.v0 := stack[top-6];
                  facep^.v1 := stack[top-3];
                  facep^.v2 := idx;
                  facep^.c := c-1;
                  //faces.push([ stack[top-6], stack[top-3], idx, c ]);
                end;

                aFaces.Add(facep);
              end;

              Dec(top, 3);
            end;
          end;

          //Push vertex
          stack[top] := idx;
          Inc(top);
          stack[top] := vert[0];
          Inc(top);
          stack[top] := vert[1];
          Inc(top);

          //Update loop index
          if n_side then
            Inc(r_i)
          else
            inc(l_i);

          side := n_side;
        end;
      end;
    end;

    Inc(d);
  end;

  Polygons.Free;

  if not aEraseDupVt then Exit;

  // 중복정점 마킹
  for i := 0 to aVertices.Count-2 do
  for j := i+1 to aVertices.Count - 1 do
  if PVxVertex(aVertices[j])^.DupIndex < 0 then
  if CompareMem(@TVxVertex(aVertices[i]^).Arr, @TVxVertex(aVertices[j]^).Arr, SizeOf(I32) * 3) then
    PVxVertex(aVertices[j])^.DupIndex := i;

  // 중복 정점을 사용한 면의 정점 인덱스 수정.
  for i := 0 to aFaces.Count - 1 do
  begin
    facep := aFaces[i];
    for j := 0 to 3 - 1 do
    begin
      vp := aVertices[facep^.Arr[j]];
      if vp^.DupIndex >= 0 then
        facep^.Arr[j] := vp^.DupIndex;
    end;
  end;

  // 중복 정점 제거. 해당 인덱스를 가진 면의 정점인덱스 감소.
  for i := aVertices.Count-1 downto 0 do
  begin
    vp := aVertices[i];
    if vp^.DupIndex >= 0 then
    begin
      for j := 0 to aFaces.Count - 1 do
      begin
        facep := aFaces[j];
        for k := 0 to 3 - 1 do
        if facep^.Arr[k] > i then
        begin
          Dec(facep^.Arr[k]);
        end;
      end;

      aVertices.Delete(i);
    end;
  end;

  // 면의 큐빅노멀 설정.
  for i := 0 to aFaces.Count - 1 do
  begin
    facep := aFaces[i];
    // 정점좌표 포인터 얻고
    for j := 0 to 3 - 1 do
      vps[j] := aVertices[facep^.Arr[j]];

    // 노멀. 0->1, 0->2 의 외적 계산.
    v0 :=
      MakeVector3f(
        vps[1]^.x - vps[0]^.x,
        vps[1]^.y - vps[0]^.y,
        vps[1]^.z - vps[0]^.z
      );
    v1 :=
      MakeVector3f(
        vps[2]^.x - vps[0]^.x,
        vps[2]^.y - vps[0]^.y,
        vps[2]^.z - vps[0]^.z
      );
    Normal := VectorNormalize3f(VectorCrossProduct3f(v0, v1));

    facep^.n := GetCubicNormalIndex(Normal);
  end;
end;

function ExtractJustName(const FileName: String): String;
begin
  Result := ExtractFileName(FileName);
  SetLength(Result, Length(Result) - Length(ExtractFileExt(FileName)));
end;

procedure BuildTexture(const aObjFileName: String);
var
  TextureFileName: String;
  Bmp: TBitmap;
  PNGImage: TPNGObject;
  i: Integer;
  VoxelColor: Voxel_Engine.TVector3f;
begin
  TextureFileName :=
    ExtractFilePath(aObjFileName) + 
    ExtractJustName(aObjFileName) + '.png';

  Bmp:= TBitmap.Create;
  PNGImage:= TPNGObject.Create;
  try
    Bmp.SetSize(16, 16);
    Bmp.PixelFormat := pf32bit;
    for i := 0 to 16*16 - 1 do
    begin
      VoxelColor := //GetVXLColor(i, 0);
                    GetCorrectColour(i, RemapColour);
      Bmp.Canvas.Pixels[i div 16, i mod 16] :=
        RGB(
          Round(VoxelColor.X * 255),
          Round(VoxelColor.Y * 255),
          Round(VoxelColor.Z * 255)
        );

    end;


    PNGImage.Assign(Bmp);
    PNGImage.SaveToFile(TextureFileName);
  finally
    Bmp.Free;
    PNGImage.Free;
  end;


end;

procedure ExportToObjFile(const aFileName: String);
var
  F: TextFile;
  Vertices: TRecords;
  Faces: TRecords;

  i: Integer;
begin
  Vertices:= TRecords.Create;
  Faces:= TRecords.Create; ;

  BuildMesh(Vertices, Faces, true);
  BuildTexture(aFileName); 

  AssignFile(F, aFileName);
  Rewrite(F);

  Writeln(F, '# Voxel Section Editor III Wavefront OBJ Exporter v0.01 - by oranke');
  Write(F, '# File Created: ');
  Write(F, DateToStr(Now));
  WriteLn(F, ' ', TimeToStr(Now));
  WriteLn(F, '');  

  for i := 0 to Vertices.Count - 1 do
  with PVxVertex(Vertices[i])^ do
    WriteLn(F, Format('v %d.0 %d.0 %d.0', [x, y, z]));
  WriteLn(F, '');
  //{
  for i := Low(CUBIC_NORMALS) to High(CUBIC_NORMALS) do
    WriteLn(F,
      Format('vn %d.0 %d.0 %d.0',
        [
          CUBIC_NORMALS[i][C_x],
          CUBIC_NORMALS[i][C_y],
          CUBIC_NORMALS[i][C_z]
        ]
      )
    );
  WriteLn(F, '');
  //}
  for i := 0 to Faces.Count - 1 do
  with PVxFace(Faces[i])^ do
    WriteLn(F, Format('f %d//%d %d//%d %d//%d', [v0+1, n+1, v1+1, n+1, v2+1, n+1]));
    //WriteLn(F, Format('f %d %d %d', [v0+1, v1+1, v2+1]));
    //WriteLn(F, Format('f %d %d %d', [Arr[0]+1, Arr[1]+1, Arr[2]+1]));
    //WriteLn(F, Format('f %d/%d/%d %d/%d/%d %d/%d/%d', [v0+1, c+1, n+1, v1+1, c+1, n+1, v2+1, c+1, n+1]));


  WriteLn(F, '');

  CloseFile(F);

  Vertices.Free;
  Faces.Free;
  
end;



initialization

finalization

end. 
