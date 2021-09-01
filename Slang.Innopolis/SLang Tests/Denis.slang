Sign(n: Real): Real => if n = 0.0 then 0.0 else n / Math.abs(n) end

unit Vector
    x, y: Real

    init() => init(0.0, 0.0)

    init(x, y: Real) do
        this.x := x
        this.y := y
    end

    init(from, to: Point) => init(to.x - from.x, to.y - from.y)

    * (that: Vector): Real => x * that.y - that.x * y

    ScalarProduct(that: Vector): Real => x * that.x + y * that.y
end

// Geometry entities

unit GeometricEntity
    pure Intersect(s: GeometricEntity): Boolean is abstract
end

unit Point extend GeometricEntity
    x, y: Real

    init() => init(0.0, 0.0)

    init(x, y: Real) do
        this.x := x
        this.y := y
    end

    SquaredDistanceTo(that: Point): Real => Math.sqr(x - that.x) + Math.sqr(y - that.y)

    pure override Intersect(p: Point): Boolean => x = p.x and y = p.y

    pure override Intersect(s: Segment): Boolean => s.Intersect(this)

    pure override Intersect(r: Rectangle): Boolean => r.Intersect(this)

    pure override Intersect(c: Circle): Boolean => c.Intersect(this)
end

unit Segment extend GeometricEntity
    p1, p2: Point

    init() => init(new Point(), new Point())

    init(p1, p2: Point) do
        this.p1 := p1
        this.p2 := p2
    end

    pure override Intersect(p: Point): Boolean do
        v1 is Vector(p1, p)
        v2 is Vector(p2, p)

        return v1.ScalarProduct(new Vector(p1, p2)) >= 0.0 and
               v2.ScalarProduct(new Vector(p2, p1)) >= 0.0 and
               v1 * new Vector(p1, p2) = 0.0
    end

    pure override Intersect(s: Segment): Boolean do
        if not new Rectangle(p1, p2).Intersect() then return false end

        v1 is Vector(p1, p2)
        v2 is Vector(s.p1, s.p2)

        return Sign(v1 * new Vector(p1, s.p1)) * Sign(v1 * new Vector(p1, s.p2)) <= 0.0 and
               Sign(v2 * new Vector(s.p1, p1)) * Sign(v2 * new Vector(s.p1, p2)) <= 0.0
    end

    pure override Intersect(r: Rectangle): Boolean => r.Intersect(this)

    pure override Intersect(c: Circle): Boolean => c.Intersect(this)
end

unit Rectangle extend GeometricEntity
    hidden topRight, bottomLeft: Point

    init() => init(new Point(), new Point())

    init(p1, p2: Point) do
        topRight := new Point(Math.max(p1.x, p2.x), Math.max(p1.y, p2.y))
        bottomLeft := new Point(Math.min(p1.x, p2.x), Math.min(p1.y, p2.y))
    end

    pure Left(): Real => bottomLeft.x
    pure Right(): Real => topRight.x
    pure Bottom(): Real => bottomLeft.y
    pure Top(): Real => topRight.y

    pure BottomLeft(): Real => bottomLeft
    pure BottomRight(): Real => new Point(topRight.x, bottomLeft.y)
    pure TopLeft(): Real => new Point(bottomLeft.x, topRight.y)
    pure TopRight(): Real => topRight

    pure override Intersect(p: Point): Boolean => Left() <= p.x and p.x <= Right() and Bottom() <= p.y and p.y <= Top()

    pure override Intersect(s: Segment): Boolean => Intersect(s.p1) or
                                                    Intersect(s.p2) or
                                                    s.Intersect(new Segment(BottomLeft(), BottomRight())) or
                                                    s.Intersect(new Segment(BottomRight(), TopRight())) or
                                                    s.Intersect(new Segment(TopRight(), TopLeft())) or
                                                    s.Intersect(new Segment(TopLeft(), BottomLeft()))

    pure override Intersect(r: Rectangle): Boolean do
        maxLeft is Math.max(Left(), r.Left())
        minRight is Math.min(Right(), r.Right())
        maxBottom is Math.max(Bottom(), r.Bottom())
        minTop is Math.min(Top(), r.Top())
        return minRight >= maxLeft && minTop >= maxBottom
    end

    pure override Intersect(c: Circle): Boolean => c.Intersect(this)
end

unit Circle extend GeometricEntity
    center: Point
    radius: Real

    init() => init(new Point(), 0.0)

    init(c: Point, r: Real) do
        this.center := c
        this.radius := r
    end

    pure override Intersect(p: Point): Boolean => center.SquaredDistanceTo(p) <= Math.sqr(radius)

    pure override Intersect(s: Segment): Boolean =>
        if new Vector(s.p1, s.p2).ScalarProduct(new Vector(s.p1, center)) >= 0.0 and new Vector(s.p2, s.p1).ScalarProduct(new Vector(s.p2, center)) >= 0.0 then
            Math.sqr(Math.abs(new Vector(s.p1, s.p2) * new Vector(s.p1, center))) <= Math.sqr(radius) * s.p1.SquaredDistanceTo(s.p2)
        else
            Math.min(center.SquaredDistanceTo(s.p1), center.SquaredDistanceTo(s.p2)) <= Math.sqr(radius)
        end

    pure override Intersect(r: Rectangle): Boolean => r.Intersect(center) or
                                                      Intersect(new Segment(r.BottomLeft(), r.BottomRight())) or
                                                      Intersect(new Segment(r.BottomRight(), r.TopRight())) or
                                                      Intersect(new Segment(r.TopRight(), r.TopLeft())) or
                                                      Intersect(new Segment(r.TopLeft(), r.BottomLeft()))

    pure override Intersect(c: Circle): Boolean => center.SquaredDistanceTo(c.center) <= Math.sqr(radius + c.radius)
end

// Game entities

unit GameEntity
    hidden geometry: GeometricEntity

    pure Collide(o: Entity): Boolean => geometry.Intersect(o.geometry)
end

unit Infantry extend GameEntity
    init(p: Point) => this.geometry := p
end

unit Building extend GameEntity
    init(r: Rectangle) => this.geometry := r
end

unit Tower extend GameEntity
    init(c: Circle) => this.geometry := c
end

unit Fence extend GameEntity
    init(s: Segment) => this.geometry := s
end
 to join this conversation on GitHub. Already hav