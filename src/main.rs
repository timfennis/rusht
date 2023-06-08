use anyhow::Context;
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet, VecDeque},
    fmt::{Display, Write},
    str::FromStr,
};

pub(crate) mod data;

use crate::data::*;

const EXIT_LANE: u8 = 2;
const GRID_SIZE: usize = 6;

#[derive(Debug, PartialEq, Clone)]
struct GameState {
    vehicles: HashSet<Vehicle>,
}

impl Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut matrix = [[0; GRID_SIZE]; GRID_SIZE];

        for (i, vehicle) in self.vehicles.iter().enumerate() {
            for (x, y) in vehicle.positions() {
                debug_assert!(x < 6, "vehicle({}) x out of bounds: {}", vehicle, x);
                debug_assert!(y < 6, "vehicle({}) y out of bounds: {}", vehicle, y);
                debug_assert_eq!(
                    0, matrix[y][x],
                    "collission in occupancy_matrix at ({},{})",
                    y, x
                );
                matrix[y][x] = i;
            }
        }
        f.write_str("--------\n")?;
        for row in matrix {
            f.write_char('|')?;
            for b in row {
                if b > 0 {
                    f.write_fmt(format_args!("{}", b))?;
                } else {
                    f.write_char(' ')?;
                }
            }
            f.write_char('|')?;
            f.write_char('\n')?;
        }
        f.write_str("--------\n")
    }
}

impl GameState {
    pub fn occupancy_matrix(&self) -> [[bool; GRID_SIZE]; GRID_SIZE] {
        let mut matrix = [[false; GRID_SIZE]; GRID_SIZE];

        for vehicle in &self.vehicles {
            for (x, y) in vehicle.positions() {
                debug_assert!(x < 6, "vehicle({}) x out of bounds: {}", vehicle, x);
                debug_assert!(y < 6, "vehicle({}) y out of bounds: {}", vehicle, y);
                debug_assert!(
                    !matrix[y][x],
                    "collission in occupancy_matrix at ({},{})",
                    y, x
                );
                matrix[y][x] = true
            }
        }

        matrix
    }

    pub fn is_solved(&self) -> bool {
        let red_car = self
            .vehicles
            .iter()
            .filter(|&v| v.kind == VehicleKind::RedCar)
            .collect::<Vec<_>>();
        debug_assert_eq!(
            red_car.len(),
            1,
            "failed asserting that only one red car exists in the game state"
        );
        let red_car = *red_car.get(0).unwrap();

        let matrix = self.occupancy_matrix();
        debug_assert_eq!(
            red_car.orientation,
            Orientation::Horizontal,
            "failed asserting that the red car is in a horizontal position"
        );

        debug_assert_eq!(
            red_car.pos.1, EXIT_LANE,
            "failed asserting that the red car is in the correct lane"
        );

        let start_x = (red_car.pos.0 + 2) as usize;
        (start_x..GRID_SIZE).all(|x| matrix[EXIT_LANE as usize][x] == false)
    }

    fn encode_as_string(&self) -> String {
        let mut vehicles = self.vehicles.iter().collect::<Vec<&Vehicle>>();
        vehicles.sort_by(|&a, &b| match a.pos.0.cmp(&b.pos.0) {
            Ordering::Equal => a.pos.1.cmp(&b.pos.1),
            ord => ord,
        });

        vehicles
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

impl FromStr for GameState {
    type Err = anyhow::Error;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let vehicles: Result<HashSet<Vehicle>, _> = str
            .chars()
            .collect::<Vec<char>>()
            .chunks(4)
            .map(|chunk| {
                chunk
                    .try_into()
                    .context("failed to split string into chunks of 4")
            })
            .collect();
        Ok(GameState {
            vehicles: vehicles?,
        })
    }
}

fn solve(state: &GameState) -> Option<(u32, Vec<GameState>)> {
    let mut seen: HashMap<String, u32> = HashMap::new();
    let mut buffer: VecDeque<(u32, GameState, Vec<GameState>)> = VecDeque::new();

    seen.insert(state.encode_as_string(), 0);
    buffer.push_back((0, state.clone(), Vec::new()));

    fn evaluate_loc(
        matrix: &[[bool; 6]; 6],
        x: usize,
        y: usize,
        moves: u32,
        current_state: &GameState,
        history: &Vec<GameState>,
        vehicle: &Vehicle,
        seen: &mut HashMap<String, u32>,
        buffer: &mut VecDeque<(u32, GameState, Vec<GameState>)>,
    ) -> bool {
        // true if blocked
        // if the car were te be placed in (x, y) would that be possible?
        // first we check all the coordinates of the car
        for (x, y) in vehicle.copy_with_xy(x as u8, y as u8).positions() {
            if matrix[y][x] == true {
                return true;
            }
        }

        let mut new_vehicles = current_state.vehicles.clone();
        new_vehicles.remove(&vehicle);
        new_vehicles.insert(vehicle.copy_with_xy(x as u8, y as u8));

        let new_state = GameState {
            vehicles: new_vehicles,
        };

        // if we've already been here but with less moves we can skip adding this new location to the buffer
        if let Some(cur_best) = seen.get(&new_state.encode_as_string()) {
            if *cur_best <= (moves + 1) {
                return false;
            }
        }

        let mut new_history = history.clone();

        seen.insert(new_state.encode_as_string(), moves + 1);
        new_history.push(new_state.clone());
        buffer.push_back((moves + 1, new_state, new_history));

        false
    }
    while let Some((moves, current_state, history)) = buffer.pop_front() {
        if current_state.is_solved() {
            // we add one extra to the moves
            return Some((moves + 1, history));
        }

        for vehicle in current_state.vehicles.iter() {
            // Todo: creating this version of the occupancy matrix is probably slow as hell
            let other_vehicles = current_state
                .vehicles
                .iter()
                .filter(|&v| v != vehicle)
                .map(|v| v.clone())
                .collect::<HashSet<_>>();

            let matrix = GameState {
                vehicles: other_vehicles,
            }
            .occupancy_matrix();
            // let matrix = current_state.occupancy_matrix();
            let (cur_x, cur_y) = (vehicle.pos.0 as usize, vehicle.pos.1 as usize);
            let len = vehicle.kind.len();
            if vehicle.orientation == Orientation::Horizontal {
                // move left
                for new_x in (0..cur_x).rev() {
                    let blocked = evaluate_loc(
                        &matrix,
                        new_x,
                        cur_y,
                        moves,
                        &current_state,
                        &history,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    );
                    if blocked {
                        break;
                    }
                }

                // move right
                for new_x in (cur_x + 1)..=(GRID_SIZE - len) {
                    let blocked = evaluate_loc(
                        &matrix,
                        new_x,
                        cur_y,
                        moves,
                        &current_state,
                        &history,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    );
                    if blocked {
                        break;
                    }
                }
            } else {
                // move up
                for new_y in (0..cur_y).rev() {
                    let blocked = evaluate_loc(
                        &matrix,
                        cur_x,
                        new_y,
                        moves,
                        &current_state,
                        &history,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    );
                    if blocked {
                        break;
                    }
                }

                // move down
                for new_y in (cur_y + 1)..=(GRID_SIZE - len) {
                    let blocked = evaluate_loc(
                        &matrix,
                        cur_x,
                        new_y,
                        moves,
                        &current_state,
                        &history,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    );
                    if blocked {
                        break;
                    }
                }
            }
        }
    }

    None
}

fn main() -> anyhow::Result<()> {
    let state = "R02HC30VC40HC22VC32VC52VC04VT14HC43VC54V".parse::<GameState>()?;
    println!("initial state: \n{}", state);
    if let Some((solution, history)) = solve(&state) {
        println!("solveable in {} moves", solution);
        for (n, state) in history.iter().enumerate() {
            println!("Move {}\n{}", n + 1, state);
        }
        println!("solveable in {} moves", solution)
    } else {
        println!("puzzle has no solution");
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_that_order_does_not_change_state() {
        let state1 = "R22HT77V".parse::<GameState>().unwrap();
        let state2 = "T77VR22H".parse::<GameState>().unwrap();

        assert_eq!(state1, state2);
    }

    #[test]
    fn test_that_different_states_are_not_equal() {
        let state1 = "R22HT77V".parse::<GameState>().unwrap();
        let state2 = "R22HT76V".parse::<GameState>().unwrap();

        assert_ne!(state1, state2);
    }

    #[test]
    fn test_that_encoding_the_game_always_produces_the_same_string() {
        let state1 = "R22HT77V".parse::<GameState>().unwrap();
        let state2 = "T77VR22H".parse::<GameState>().unwrap();

        assert_eq!(state1.encode_as_string(), state2.encode_as_string());
    }
}
