class Cylinder {
  PVector location;
  static final float cylinderHeight = 50;
  static final  float   cylinderBaseSize = 20;
  static final  int cylinderResolution = 40;


  Cylinder() {}

  Cylinder(PVector loc) {
    location = loc;
  }

void constructCylinder() {
    float angle;
    float[] x = new float[cylinderResolution  +1];
    float[] y = new float[cylinderResolution +1];
    for (int i = 0; i< x.length; i++) {
      angle = (TWO_PI / cylinderResolution) *i;
      x[i] = sin(angle) * cylinderBaseSize;
      y[i] = cos(angle) * cylinderBaseSize;
    }
    openCylinder = createShape();
    openCylinder.beginShape(QUAD_STRIP);

    surfaceCylinder0 = createShape();
    surfaceCylinder0.beginShape(TRIANGLE_FAN);
    surfaceCylinderH = createShape();
    surfaceCylinderH.beginShape(TRIANGLE_FAN);

    surfaceCylinder0.vertex(0, -thickPlate*2, 0);
    surfaceCylinderH.vertex(0, cylinderHeight-thickPlate*2, 0);
    for (int i = 0; i < x.length; i++) {
      openCylinder.vertex(x[i], -thickPlate*2, y[i]);
      openCylinder.vertex(x[i], cylinderHeight-thickPlate*2, y[i]);
      surfaceCylinder0.vertex(x[i], -thickPlate*2, y[i]);
      surfaceCylinderH.vertex(x[i], cylinderHeight-thickPlate*2, y[i]);
    }

    openCylinder.endShape();
    surfaceCylinder0.endShape();
    surfaceCylinderH.endShape();
  }


  /*void drawCylinder() {
    translate(mouseX-(width/2), thick-50, mouseY-(height/2));
    locPos = new PVector(mouseX-(width/2), thick-50, mouseY-(height/2));
    rotateX(-PI/2);

    shape(openCylinder);

    shape(closeCylinder);
  }

  void display(PVector loc) {
  }*/
}