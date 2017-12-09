class Mover {
  PVector location;
  PVector velocity;
  PVector friction;
  PVector gravityForce;

  final float mu = 0.2;
  final float normalForce =1;
  final float frictionMagnitude = normalForce*mu;
  final float gravityConstant = 1;

  float radius = 10;

  Mover() {
    location = new PVector(0, 0, 0);
    velocity = new PVector(0, 0, 0);
  }
  void update() {
    gravityForce = new PVector(
      sin(ry) * gravityConstant, 
      0, 
      -sin(rz) * gravityConstant);

    friction = velocity.get();
    friction.mult(-1);
    friction.normalize();
    friction.mult(frictionMagnitude);

    velocity.add(gravityForce);
    velocity.add(friction);
    location.add(velocity);
  }

  void display() {
    //noStroke();
    //strokeWeight(2);
    translate(location.x, 0, location.z);
    sphere(radius);
  }

  void checkEdges() {
    if (location.x > (halfPlateSize)) {
      velocity.x *= -1;
      location.x = halfPlateSize;
    }
    if (location.z > (halfPlateSize)) {
      velocity.z *= -1;
      location.z = halfPlateSize;
    }
    if (location.x < -(halfPlateSize)) {
      velocity.x *= -1;
      location.x = -(halfPlateSize);
    }
    if (location.z < -(halfPlateSize)) {
      velocity.z *= -1;
      location.z = -(halfPlateSize);
    }
  }
  
  void checkCylinderCollision() {
    float avoidDist = cylinderBaseSize + radius;
    for (Cylinder c : cylinders) {
     
      float cX = c.location.x - width/2;
      float cZ = c.location.z - height/2;
      PVector dist = new PVector(location.x-cX, 0, location.z -cZ);
      if(avoidDist >= sqrt(dist.x * dist.x + dist.z * dist.z) ) {
        PVector n = dist.normalize();
      location.x = cX + n.x * (avoidDist);
      location.z = cZ + n.z * (avoidDist);
      velocity.sub(n.mult(2*velocity.dot(n)));
      }
    }
  }
}