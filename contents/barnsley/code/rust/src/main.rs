use rand::prelude::*;
#[derive(Clone, Copy)]
struct Point2 {
    x: f64,
    y: f64,
}

#[derive(Clone, Copy)]
struct Point3 {
    x: f64,
    y: f64,
    z: f64,
}

impl Point3 {
    fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }

    fn matrix_mul(self, rhs: Vec<Point3>) -> Self {
        let x = rhs[0].x * self.x + rhs[0].y * self.y + rhs[0].z * self.z;
        let y = rhs[1].x * self.x + rhs[1].y * self.y + rhs[1].z * self.z;
        let z = rhs[2].x * self.x + rhs[2].y * self.y + rhs[2].z * self.z;
        Self::new(x, y, z)
    }
}

fn select_array(hutchinson_op: &[Vec<Point3>], probabilities: &[f64]) -> Vec<Point3> {
    let mut rng = rand::thread_rng();
    let mut rnd = rng.gen::<f64>();

    for (i, probability) in probabilities.iter().enumerate() {
        if rnd < *probability {
            return hutchinson_op[i].clone();
        }
        rnd -= probability;
    }

    return vec![];
}

fn chaos_game(
    iters: usize,
    initial_location: Point2,
    hutchinson_op: &[Vec<Point3>],
    probabilities: &[f64],
) -> Vec<Point2> {
    let mut point = Point3 {
        x: initial_location.x,
        y: initial_location.y,
        z: 1.0,
    };
    (0..iters)
        .into_iter()
        .map(|_| {
            let old_point = point;
            let operation = select_array(hutchinson_op, probabilities);
            point = point.matrix_mul(operation);
            Point2 {
                x: old_point.x,
                y: old_point.y,
            }
        })
        .collect()
}

fn main() {
    let barnsley_hutchinson = vec![
        vec![
            Point3::new(0.0, 0.0, 0.0),
            Point3::new(0.0, 0.16, 0.0),
            Point3::new(0.0, 0.0, 1.0),
        ],
        vec![
            Point3::new(0.85, 0.04, 0.0),
            Point3::new(-0.04, 0.85, 1.60),
            Point3::new(0.0, 0.0, 1.0),
        ],
        vec![
            Point3::new(0.20, -0.26, 0.0),
            Point3::new(0.23, 0.22, 1.60),
            Point3::new(0.0, 0.0, 1.0),
        ],
        vec![
            Point3::new(-0.15, 0.28, 0.0),
            Point3::new(0.26, 0.24, 0.44),
            Point3::new(0.0, 0.0, 1.0),
        ],
    ];

    let barnsley_probabilities = vec![0.01, 0.85, 0.07, 0.07];

    let mut out = String::new();

    for point in chaos_game(
        10_000,
        Point2 { x: 0.0, y: 0.0 },
        &barnsley_hutchinson,
        &barnsley_probabilities,
    ) {
        out += format!("{}\t{}\n", point.x, point.y).as_str();
    }

    std::fs::write("./out.dat", out).unwrap();
}
