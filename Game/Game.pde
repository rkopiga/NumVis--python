Mover ball;
PShape openCylinder = new PShape();
PShape surfaceCylinder0 = new PShape();
PShape surfaceCylinderH = new PShape();
  static final float cylinderHeight = 50;
  static final  float   cylinderBaseSize = 20;
  static final  int cylinderResolution = 40;
ArrayList<Cylinder> cylinders = new ArrayList<Cylinder>();
int window = 600;
float diff = 1;

void settings() {
  size(window, window, P3D);
}

void setup() {
  noStroke();
  ball = new Mover();
}

float plateSpeed = 0.7; //speed of turning the plate
float plateSize = 400;
float halfPlateSize = plateSize/2;
float thickPlate = 5;


void draw() {
  background(200);

  if (!shiftPressed) { 
    gamePlay();
  } else {
    gameStop();
  }
}

void gamePlay() { //playing game, view on plate sideways
  directionalLight(50, 100, 125, 0, 1, 0); // light from top

  pushMatrix();
  translate(width/2, height/2, -100);
  rotateX(rz);
  rotateZ(ry);
  ambientLight(50, 100, 40); //green
  box(plateSize, thickPlate, plateSize);

  noStroke();
  translate(-width/2, 0, -height/2);
  drawCylinders(); //those already created
  translate(width/2, -thickPlate/2-ball.radius, height/2);  // 0 on the plate

  ambientLight(50, 50, 200); //light blue
  ball.update(); //move the ball
  ball.checkCylinderCollision(); //bound
  ball.checkEdges(); //bound the ball
  ball.display();

  popMatrix();
}

void gameStop() {//SHIFT pressed, view on plate from the top
  directionalLight(50, 100, 125, 0, 0, -1); //light from the player

  pushMatrix();
  translate(width/2, height/2, 0); //0 in center of window
  rotateX(-PI/2); //rotate the plate to look at it from the top

  ambientLight(50, 100, 40); //green
  box(2*halfPlateSize, thickPlate, 2*halfPlateSize); //plate

  translate(0, -thickPlate/2-ball.radius, 0); //0 on the plate
  ambientLight(50, 50, 200); //light blue
  ball.display(); //ball doesn't update, just displayed

  popMatrix();
  pushMatrix();

  rotateX(-PI/2);
  drawCylinders(); //those already created

  if (pressedButtonLeft) {
    newCylinder(); //create new cylinder if the left button of the mouse is pressed
  }
  popMatrix();
}

//draw cylinders already created
void drawCylinders() {
  ambientLight(120, 30, 10); //yellow

  for (Cylinder c : cylinders) {
    pushMatrix();
    translate(c.location.x, -(cylinderHeight-thickPlate/2), c.location.z);

    shape(openCylinder);
    shape(surfaceCylinder0);
    shape(surfaceCylinderH);

    translate(-c.location.x, cylinderHeight-thickPlate/2, -c.location.z);
    popMatrix();
  }
}

//create a new cylinder
void newCylinder() {
  pushMatrix();

  //if cylinder can be created here
  if (checkPosition(creatingCylinderX, creatingCylinderZ, cylinderBaseSize)) {
    Cylinder cyl = new Cylinder(new PVector(creatingCylinderX, 0, creatingCylinderZ)); //create new cylinder with position of the mouse
    cyl.constructCylinder();
    cylinders.add(cyl); //ads the cylinder in the array

    float positionx = creatingCylinderX + width/2;
    float positionz = creatingCylinderZ + height/2;

    ambientLight(120, 30, 10); //yellow

    //place the new cylinder on the plate
    translate(positionx, -cylinderHeight-thickPlate/2, positionz);

    shape(openCylinder);
    shape(surfaceCylinder0);
    shape(surfaceCylinderH);

    //return to the previous 0
    translate(-positionx, cylinderHeight+thickPlate/2, -positionz);
  }
  popMatrix();
}

float distX, distY;
float ry, rz;
void mouseDragged()  //turns the plate
{
  distY = -(mouseY-pmouseY);
  distX = (mouseX-pmouseX);
  float maxAngle = PI/3.0; //limits of turning the plate
  ry += map(distX * plateSpeed, 0, width, 0, maxAngle);
  rz += map(distY * plateSpeed, 0, height, 0, maxAngle);
  if (rz > maxAngle) { 
    rz = maxAngle;
  }
  if (rz < -maxAngle) { 
    rz = -maxAngle;
  }
  if (ry > maxAngle) { 
    ry = maxAngle;
  }
  if (ry < -maxAngle) { 
    ry = -maxAngle;
  }
}

void mouseWheel(MouseEvent e) { //changes speed of turning the plate
  plateSpeed += e.getCount();
  if (plateSpeed > 1.5) {
    plateSpeed = 1.5;
  }
  if (plateSpeed < 0.2) {
    plateSpeed = 0.2;
  }
}

boolean shiftPressed = false;
void keyPressed() {
  if (key == CODED && keyCode == SHIFT) {
    shiftPressed = true;
  }
}

void keyReleased() {
  if (key == CODED && keyCode == SHIFT) {
    shiftPressed = false;
  }
}

int creatingCylinderX, creatingCylinderZ;
boolean pressedButtonLeft = false;
void mousePressed() {
  if (mouseButton == LEFT && shiftPressed) { // if mouse pressed only in gameSTOP
    pressedButtonLeft = true;
    creatingCylinderX = mouseX;
    creatingCylinderZ = mouseY;
  }
}

boolean  checkPosition(int x, int z, float object) {
  boolean canBeCreated = false;
  float avoidDist = cylinderBaseSize + object;

  //create a new cylinder only on the plate
  if (x >= width/2 - halfPlateSize + object &&
    x <= width/2 + halfPlateSize - object &&
    z >= height/2 - halfPlateSize + object &&
    z <= height/2 + halfPlateSize - object) {
    canBeCreated = true;

    //don't allow to create a new cylinder if there's an other cylinder
    for (Cylinder c : cylinders) {
      PVector distanceC = new PVector(x-c.location.x, 0, z - c.location.z);
      if (avoidDist >= sqrt(distanceC.x * distanceC.x + distanceC.z * distanceC.z) ) {
        canBeCreated = false;
      }
    }

    //don't allow to create a new cylinder if there's an other cylinder
    PVector distanceB = new PVector(x - width/2 -ball.location.x, 0, z - height/2 - ball.location.z);
    if (ball.radius + object > sqrt(distanceB.x * distanceB.x + distanceB.z * distanceB.z)) {
      canBeCreated = false;
    }
  }
  return canBeCreated;
}

void mouseReleased() {
  pressedButtonLeft = false;
}