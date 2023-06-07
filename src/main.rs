use anyhow::{anyhow, Context};
use std::{collections::HashSet, str::FromStr};

#[derive(Debug, PartialEq, Eq, Hash)]
struct Vehicle {
    pos: (u8, u8),
    orientation: Orientation,
    kind: VehicleKind,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Orientation {
    Horizontal,
    Vertical,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum VehicleKind {
    Truck,
    Car,
    RedCar,
}

#[derive(Debug, PartialEq)]
struct GameState {
    vehicles: HashSet<Vehicle>,
}

impl FromStr for GameState {
    type Err = anyhow::Error;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let vehicles: Result<HashSet<Vehicle>, _> = str
            .chars()
            .collect::<Vec<char>>()
            .chunks(4)
            .map(|chunk| chunk.try_into())
            .collect();
        Ok(GameState {
            vehicles: vehicles?,
        })
    }
}

impl TryFrom<&[char]> for Vehicle {
    type Error = anyhow::Error;

    fn try_from(value: &[char]) -> Result<Self, Self::Error> {
        let value: [char; 4] = value.try_into()?;
        let kind = match value[0] {
            'R' => VehicleKind::RedCar,
            'C' => VehicleKind::Car,
            'T' => VehicleKind::Truck,
            _ => return Err(anyhow!("invalid vehicle type")),
        };

        let x = value[1]
            .to_digit(10)
            .map(|d| d as u8)
            .context("invalid x coordinate")?;
        let y = value[2]
            .to_digit(10)
            .map(|d| d as u8)
            .context("invalid y coordinate")?;

        let orientation = match value[3] {
            'H' => Orientation::Horizontal,
            'V' => Orientation::Vertical,
            _ => return Err(anyhow!("invalid or missing orientation")),
        };

        Ok(Vehicle {
            pos: (x, y),
            orientation,
            kind,
        })
    }
}

fn main() -> anyhow::Result<()> {
    let state = "R33HT88V".parse::<GameState>()?;
    dbg!(state);
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_that_order_does_not_change_state() {
        let state1 = "R33HT77V".parse::<GameState>().unwrap();
        let state2 = "T77VR33H".parse::<GameState>().unwrap();

        assert_eq!(state1, state2);
    }

    #[test]
    fn test_that_different_states_are_not_equal() {
        let state1 = "R33HT77V".parse::<GameState>().unwrap();
        let state2 = "R33HT76V".parse::<GameState>().unwrap();

        assert_ne!(state1, state2);
    }
}
