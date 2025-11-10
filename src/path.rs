//as an example, below is a schedule that could be entered in by the user,
/*
Y25-26 Semester 1
Exp 	Trm 	Crs-Sec 	Course Name 	Teacher 	Room 	Enroll 	Leave
1-2(A,C) 	S1 	SCI603-1 	Biology: Molecular & Cellular 	O'Leary-Driscoll, Sarah 	B108 	08/04/2025 	01/05/2026
3(A-D) 	S1 	MAT331-1 	BC Calculus III 	Trimm, Anderson 	A135 	08/04/2025 	01/05/2026
4(A-D) 	S1 	CS255-2 	Elements of Computing Systems 1 	Campbell, Dan K. 	A152 	08/04/2025 	01/05/2026
5(A-B,D) 	S1 	ENG201a-2 	Literary Explorations III: American 	Kotlarczyk, Adam C 	A119 	08/04/2025 	01/05/2026
6(A-D) 	S1 	SCI445-2 	Modern Physics 	Hawker, Eric 	B115 	08/04/2025 	01/05/2026
SE(A-D) 	S1 	SE001-101 	Support & Engagement 	Staff, New 	N/A 	08/18/2025 	01/05/2026
7(A-B, D) 	S1 	HSS201i-2 	Revolutions 	Buck, Patrick D. 	A114 	08/04/2025 	01/05/2026
8(A-D) 	S1 	WLG150-101 	French V 	Kwiatkowski, Anne 	A121 	08/04/2025 	01/05/2026
RC(A-Sp) 	Y25-26 	SLD130-7 	Rolling Check 	Staff, Residential Life 		08/18/2025 	06/16/2026
CC(A-Sp) 	Y25-26 	SLD120-7 	Curfew Check 	Staff, Residential Life 		08/18/2025 	06/16/2026
SIR(I) 	S1 	SIR099-999 	SIR 	Staff, SIR 	B131 	08/04/2025 	01/05/2026
EVE(A-Sp) 	Y25-26 	SLD100-28 	Residential Life 	Brown, Tyson 		08/18/2025 	06/16/2026

Y25-26 Semester 2
Exp 	Trm 	Crs-Sec 	Course Name 	Teacher 	Room 	Enroll 	Leave
1(A-B,D) 	S2 	HSS202-1 	The World in the Twentieth Century 	Eysturlid, Lee 	A147 	01/05/2026 	06/16/2026
2(A,C-D) 	S2 	WEL312-1 	Dance 	Myers, Mary Jane 	F100A 	01/05/2026 	06/16/2026
3(A-D) 	S2 	CS260-1 	Elements of Computing Systems 2 	Campbell, Dan K. 	A133 	01/05/2026 	06/16/2026
4(A-D) 	S2 	SCI425-1 	Planetary Science 	Hawker, Eric 	B115 	01/05/2026 	06/16/2026
5(A-B,D) 	S2 	ENG341-4 	Gender Studies 	Ott, Ashley 	A117 	01/05/2026 	06/16/2026
SE(A-D) 	S2 	SE001-201 	Support & Engagement 	Staff, New 	N/A 	01/05/2026 	06/16/2026
7(A-D) 	S2 	MAT442-3 	Multi-Variable Calculus 	Fogel, Micah 	A155 	01/05/2026 	06/16/2026
8(A-D) 	S2 	WLG150-201 	French V 	Kwiatkowski, Anne 	A121 	01/05/2026 	06/16/2026
RC(A-Sp) 	Y25-26 	SLD130-7 	Rolling Check 	Staff, Residential Life 		08/18/2025 	06/16/2026
CC(A-Sp) 	Y25-26 	SLD120-7 	Curfew Check 	Staff, Residential Life 		08/18/2025 	06/16/2026
EVE(A-Sp) 	Y25-26 	SLD100-28 	Residential Life 	Brown, Tyson 		08/18/2025 	06/16/2026
*/
use lazy_static::lazy_static;
use log::error;
use regex::Regex;

use crate::{closest_pair_between, name_to_id, pathfinding, Node};

struct ScheduleInfo {
    mods: Vec<String>,
    semester: Vec<String>,
    short_name: Vec<String>,
    long_name: Vec<String>,
    teacher: Vec<String>,
    room: Vec<String>,
    start: Vec<String>,
    end: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone, serde::Serialize)]
enum Semester {
    S1,
    S2,
    Year,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone, serde::Serialize)]
enum Day {
    A,
    B,
    C,
    D,
    I,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, serde::Serialize)]

// Nodes can have different locations. Entrances, exits, classrooms, and Lexington.
pub enum Location {
    Entrance,
    Exit,
    Classroom(Class),
    Lexington,
}
#[derive(Debug, serde::Serialize, Clone)]
pub struct BasicPathway {
    pub start: usize,
    pub end: usize,

    pub start_spot: Location,
    pub end_spot: Location,
}

pub type FullPathway = (Vec<usize>, (Option<Class>, Option<Class>));


// Contains all the information from Powerschool input
#[derive(Debug, PartialEq, Eq, Hash, Clone, serde::Serialize)]
pub struct Class {
    days: Vec<Day>,
    mods: Vec<u8>,
    semester: Semester,
    short_name: String,
    long_name: String,
    teacher: String,
    room: String,
    start: String,
    end: String,
}

#[derive(Debug, serde::Serialize)]
pub struct DailyNode {
    pub anode: Option<Vec<FullPathway>>,
    pub bnode: Option<Vec<FullPathway>>,
    pub cnode: Option<Vec<FullPathway>>,
    pub dnode: Option<Vec<FullPathway>>,
}

pub enum EnterExit {
    WestMain,
    EastMain,
    D13,
    D6,
}

// Regular Expressions for the Powerschool input
lazy_static! {
    static ref DAY_REGEX: Regex = Regex::new(r"^[ ABCDI,-]*$").unwrap();
    static ref MODS_REGEX: Regex = Regex::new(r"^[a-zA-Z0-9-]*$").unwrap();
    static ref CURREG_REGEX: Regex = Regex::new(r"^[\w-]+\([^()]*\)( [\w-]+\([^()]*\))*$").unwrap();
    static ref MODS_DAY_REGEX: Regex = Regex::new(r"\d+-?\d*\([^)]+\)|\d+\([^)]+\)").unwrap();
}
// Splits the input into two strings: one from the first and one from the second semester
fn split_semesters(input: &str) -> (String, String) {
    let (mut sem1, mut sem2) = (Vec::new(), Vec::new());
    let mut in_sem2 = false;

    for line in input.lines() {
        if line.contains("Semester 2") {
            in_sem2 = true;
        }
        if in_sem2 {
            sem2.push(line);
        } else {
            sem1.push(line);
        }
    }

    (sem1.join("\n"), sem2.join("\n"))
}

// Takes the user input and resolves both semesters, returning them
pub fn get_schedule(input: &str) -> (Result<Vec<Class>, String>, Result<Vec<Class>, String>) {
    let (sem1, sem2) = split_semesters(input);
    if sem1.is_empty() && sem2.is_empty() {
        return (
            Err(
                "Did you actually input anything? Make sure you copy and paste your schedule in!"
                    .to_owned(),
            ),
            Ok(Vec::new()),
        );
    };
    if sem1.is_empty() || sem2.is_empty() {
        return (
            Err("Please provide both semesters".to_owned()),
            Ok(Vec::new()),
        );
    }

    (resolve_semester(&sem1), resolve_semester(&sem2))
}

// Convert the values of a single semester string into a operable structure
fn resolve_semester(input: &str) -> Result<Vec<Class>, String> {
    let mut listvec: Vec<&str> = input
        .lines()
        .skip_while(|line| line.trim().is_empty() || line.contains("Semester"))
        .skip_while(|line| line.contains("Teacher") && line.contains("Crs-Sec"))
        .collect();

    if listvec.len() <= 2 {
        return Err("Not enough lines".to_owned());
    }
    // Cut out RC/CC/SIR etc entries
    listvec.retain(|line| {
        !line.trim().is_empty() && !line.starts_with("RC") && !line.starts_with("CC") && !line.starts_with("SIR") &&  !line.starts_with("EVE")
    });
    // Vector for each mod/semester/name/etc
    let (
        mut mods,
        mut semester,
        mut short_name,
        mut long_name,
        mut teacher,
        mut room,
        mut start,
        mut end,
    ) = (
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
    );
    // For each of the class values:
    for (num, line) in listvec.into_iter().enumerate() {
        let line = line.replace("    ", "\t");
        let split: Vec<String> = line
            .trim()
            .split('\t')
            .map(|s| s.trim().to_string())
            .collect();
        let mut roomche: String = split[5].clone();
        if roomche.contains("/") {
            roomche = roomche.split('/').collect::<Vec<&str>>()[0].to_owned();
        }
        // Add to the info list
        if split.len() == 8 {
            mods.push(split[0].clone());
            semester.push(split[1].clone());
            short_name.push(split[2].clone());
            long_name.push(split[3].clone());
            teacher.push(split[4].clone());
            room.push(roomche);
            start.push(split[6].clone());
            end.push(split[7].clone());
        } else {
            return Err(format!("Not enough arguments provided in line {num}"));
        }
    }
    // Now we have the info for the classes and such
    sort_by_day(&ScheduleInfo {
        mods,
        semester,
        short_name,
        long_name,
        teacher,
        room,
        start,
        end,
    })
}

fn parse_day(day_str: &str) -> Result<Vec<Day>, String> {
    if !DAY_REGEX.is_match(day_str) {
        error!("Invalid day value: {day_str}");
        return Err(format!("Invalid day value: {day_str}"));
    }

    let mut days = vec![];
    for day in day_str.split(',').map(str::trim) {
        match day {
            "A" => days.push(Day::A),
            "B" => days.push(Day::B),
            "C" => days.push(Day::C),
            "D" => days.push(Day::D),
            "I" => days.push(Day::I),
            _ if day.contains('-') => {
                let (start, end) = day
                    .split_once('-')
                    .ok_or_else(|| { let res= format!("Invalid range: '{day}' while parsing '{day_str}'. Try to copy and paste the schedule in again."); error!("{}", res); res})?;
                let start = start
                    .chars()
                    .next()
                    .ok_or_else(|| { let res = format!("Invalid start day: '{day}' while parsing '{day_str}'. Try to copy and paste the schedule in again."); error!("{res}"); res})?;
                let end = end.chars().next().unwrap_or(start);
                days.extend((start..=end).filter_map(|d| match d {
                    'A' => Some(Day::A),
                    'B' => Some(Day::B),
                    'C' => Some(Day::C),
                    'D' => Some(Day::D),
                    'I' => Some(Day::I),
                    _ => None,
                }));
            }
            _ => {
                let fort = format!("Unknown day pattern '{day}' while parsing '{day_str}'. Try to copy and paste the schedule in again.");
                error!("{}", fort);
                return Err(fort);
            }
        }
    }
    Ok(days)
}

fn parse_mods(mod_str: &str) -> Result<Vec<u8>, String> {
    if !MODS_REGEX.is_match(mod_str) {
        let err = format!("Invalid mod value: {mod_str}");
        error!("{}", err);
        return Err(err);
    }

    Ok(mod_str
        .split('-')
        .map(|part| {
            let (start, end) = (part.chars().next(), part.chars().last());
            match (
                start.and_then(|c| c.to_digit(10)),
                end.and_then(|c| c.to_digit(10)),
            ) {
                (Some(start), Some(end)) => (start..=end).map(|m| m as u8).collect::<Vec<u8>>(),
                _ => vec![],
            }
        })
        .collect::<Vec<Vec<u8>>>()
        .into_iter()
        .flatten()
        .collect::<Vec<u8>>())
}
fn sort_by_day(schedule_info: &ScheduleInfo) -> Result<Vec<Class>, String> {
    schedule_info
        .mods
        .iter()
        .enumerate()
        .map(|(item, mods_days)| {
            let parsed_mods_days = if CURREG_REGEX.is_match(mods_days) {
                MODS_DAY_REGEX.find_iter(mods_days)
                    .map(|mat| {
                        let (mod_str, day_str) = mat.as_str().split_once('(').ok_or_else(|| {let thing = "Could not split by delimiter '(' "; error!("{thing} {}", mat.as_str()); thing})?;
                        Ok((parse_day(day_str.trim_end_matches(')'))?, parse_mods(mod_str)?))
                    })
                    .collect::<Result<Vec<_>, String>>()?
            } else {
                let (mod_str, day_str) = mods_days.split_once('(')
                    .ok_or_else(|| { let err = format!("There was no \"(\" token in line {item}. The problematic input was {mods_days}"); error!("{err}"); err})?;
                vec![(parse_day(day_str.trim_end_matches(')'))?, parse_mods(mod_str)?)]
            };

            let (all_days, all_mods) = parsed_mods_days.into_iter().fold(
                (Vec::new(), Vec::new()),
                |(mut days, mut mods), (d, m)| {
                    days.extend(d);
                    mods.extend(m);
                    (days, mods)
                },
            );

            Ok(Class {
                days: all_days,
                mods: all_mods,
                semester: match schedule_info.semester[item].as_str() {
                    "S1" => Semester::S1,
                    "S2" => Semester::S2,
                    "Y24-25" => Semester::Year,
                    _ => return Err(format!("Unknown semester '{}'", schedule_info.semester[item])),
                },
                short_name: schedule_info.short_name[item].clone(),
                long_name: schedule_info.long_name[item].clone(),
                teacher: schedule_info.teacher[item].clone(),
                room: schedule_info.room[item].clone(),
                start: schedule_info.start[item].clone(),
                end: schedule_info.end[item].clone(),
            })
        })
        .collect()
}

pub fn path(weekly_schedule: &Vec<Class>) -> Result<[[&Class; 8]; 5], String> {
    static DEFAULT_CLASS: Class = Class {
        days: Vec::new(),
        mods: Vec::new(),
        semester: Semester::Year,
        short_name: String::new(),
        long_name: String::new(),
        teacher: String::new(),
        room: String::new(),
        start: String::new(),
        end: String::new(),
    };

    let mut weekly_path: [[&Class; 8]; 5] = [[&DEFAULT_CLASS; 8]; 5];

    for (day, _item) in weekly_path.clone().iter_mut().enumerate() {
        let letter_day: Day = match day {
            0 => Day::A,
            1 => Day::B,
            2 => Day::I,
            3 => Day::C,
            4 => Day::D,
            _ => {
                return Err(format!(
                    "If you are seeing this, things went really bad. 
                    DEBUG VAL {day}"
                ))
            }
        };

        for class in weekly_schedule {
            if class.days.contains(&letter_day) {
                for &mod_num in &class.mods {
                    let index = mod_num as usize - 1;
                    if index < 8 {
                        weekly_path[day][index] = class;
                    } else {
                        error!("Unexpected mod value --> {mod_num}");
                        return Err(format!("Unexpected mod value --> {mod_num}"));
                    }
                }
            }
        }
    }

    Ok(weekly_path)
}
pub fn node_find_func(
    schedule: &[[&Class; 8]; 5],
    mut nodes: Vec<Node>,
    entrance: &EnterExit,
    exit: &EnterExit,
    checked: bool,
) -> Result<(DailyNode, Vec<Node>), String> {
    let mut master_vec: Vec<Vec<BasicPathway>> = Vec::new();

    for (count, day) in schedule.iter().enumerate() {
        //skip I day
        if count == 2 {
            continue;
        }

        let mut clean_classes: Vec<Option<Class>> = Vec::new();
        for (itermark, class) in day.iter().enumerate() {
            let r_class = *class;
            if !class.room.trim().is_empty() {
                clean_classes.push(Some(r_class.clone()));
            }
            if itermark == 3 && checked {
                clean_classes.push(None)
            }
        }
        if count == 2 { /*println!("{:#?}", clean_classes);*/ }

        let mut day_vec: Vec<BasicPathway> = Vec::new();
        for (num, rawclass) in clean_classes.iter().enumerate() {
            if clean_classes.len() == 1 || num == clean_classes.len() - 1 {
                continue;
            };

            if count == 2 {
                println!(
                    "Looking for a connection for class {:#?}. num is {num}",
                    rawclass
                );
            }
            match rawclass {
                Some(class) => {
                    if class.mods.contains(&4) && checked {
                        continue;
                    }
                    if num == 0 {
                        let first_class = name_to_id(&class.room.trim().to_lowercase(), &nodes)?;
                        let entrance_id = match entrance {
                            EnterExit::WestMain => 988,
                            EnterExit::EastMain => 987,
                            EnterExit::D13 => 691,
                            EnterExit::D6 => 692,
                        };
                        day_vec.push(BasicPathway {
                            start: entrance_id,
                            end: first_class,
                            start_spot: Location::Entrance,
                            end_spot: Location::Classroom(class.clone()),
                        });
                        //normal

                        if clean_classes[num + 1] != None {
                            let next_class: &Class = &get_next_class(clean_classes.clone(), num);

                            let (start_room, next_room) = closest_pair_between(
                                &class.room.trim().to_lowercase(),
                                &next_class.room.trim().to_lowercase(),
                                &nodes,
                            )
                            .ok_or_else(|| {
                                match name_to_id(&class.room.trim().to_lowercase(), &nodes) {
                                    Ok(_) => {
                                        error!(
                                            "Unable to find a node match for room {:?}",
                                            next_class
                                        );
                                        format!("Could not match room '{}'", next_class.room)
                                    }
                                    Err(_) => {
                                        error!(
                                            "Unable to find a node match for room {:?}",
                                            class
                                        );
                                        format!("Could not match room '{}'", class.room,)
                                    }
                                }
                            })?;

                            if start_room != next_room {
                                day_vec.push(BasicPathway {
                                    start: start_room,
                                    end: next_room,
                                    start_spot: Location::Classroom(class.clone()),
                                    end_spot: Location::Classroom(next_class.clone()),
                                });
                            }
                        }
                    } else {
                        let next_class: &Class = &get_next_class(clean_classes.clone(), num);

                        let (start_room, next_room) = closest_pair_between(
                            &class.room.trim().to_lowercase(),
                            &next_class.room.trim().to_lowercase(),
                            &nodes,
                        )
                        .ok_or_else(|| {
                            match name_to_id(&class.room.trim().to_lowercase(), &nodes) {
                                Ok(_) => {
                                    error!(
                                        "unable to find a node match for room {:?}",
                                        next_class
                                    );

                                    format!("Could not match room '{}'", next_class.room)
                                }
                                Err(_) => {
                                    error!("Unable to find a node match for room {:?}", class);
                                    format!("Could not match room '{}'", class.room,)
                                }
                            }
                        })?;

                        if start_room != next_room {
                            day_vec.push(BasicPathway {
                                start: start_room,
                                end: next_room,
                                start_spot: Location::Classroom(class.clone()),
                                end_spot: Location::Classroom(next_class.clone()),
                            });
                        }
                    }
                    fn get_next_class(clean_classes: Vec<Option<Class>>, num: usize) -> Class {
                        let mut incre = 1;
                        if clean_classes[num + incre] == None {
                            incre = 2
                        };

                        match clean_classes[num + incre].clone() {
                            Some(res) => return res,
                            None => panic!("with incre {incre} and \n{:#?}", clean_classes),
                        }
                    }
                }
                None => {
                    let start_class;
                    match clean_classes[num.saturating_sub(1)].clone() {
                        Some(thing) => {
                            start_class = thing;
                            let start_id =
                                name_to_id(&start_class.room.trim().to_lowercase(), &nodes)?;
                            day_vec.push(BasicPathway {
                                start: start_id,
                                end: 354,
                                start_spot: Location::Classroom(start_class),
                                end_spot: Location::Lexington,
                            });
                            let end_class = clean_classes[num + 1].clone().ok_or("For some reason, clean_classes has two Nones. This should be impossible.")?;
                            let end_id = name_to_id(&end_class.room.trim().to_lowercase(), &nodes)?;
                            day_vec.push(BasicPathway {
                                start: 354,
                                end: end_id,
                                start_spot: Location::Lexington,
                                end_spot: Location::Classroom(end_class),
                            });
                        }
                        None => {
                            let entrance_id = match entrance {
                                EnterExit::WestMain => 988,
                                EnterExit::EastMain => 987,
                                EnterExit::D13 => 691,
                                EnterExit::D6 => 692,
                            };
                            day_vec.push(BasicPathway {
                                start: entrance_id,
                                end: 354,
                                start_spot: Location::Entrance,
                                end_spot: Location::Lexington,
                            });
                            let end_class = clean_classes[num + 1]
                                .clone()
                                .ok_or("Final class was a None")?;
                            let end_id = name_to_id(&end_class.room.trim().to_lowercase(), &nodes)?;
                            day_vec.push(BasicPathway {
                                start: 354,
                                end: end_id,
                                start_spot: Location::Lexington,
                                end_spot: Location::Classroom(end_class),
                            });
                        }
                    }
                }
            }
        }

        let exit_id = match exit {
            EnterExit::WestMain => 988,
            EnterExit::EastMain => 987,
            EnterExit::D13 => 691,
            EnterExit::D6 => 692,
        };

        let final_class = clean_classes
            .last()
            .ok_or("clean_classes had no last()")?
            .clone()
            .ok_or("clean_classes was None")?;

        day_vec.push(BasicPathway {
            start: name_to_id(&final_class.room.trim().to_lowercase(), &nodes)?,
            end: exit_id,
            start_spot: Location::Classroom(final_class),
            end_spot: Location::Exit,
        });
        master_vec.push(day_vec);
    }

    let mut daily_node = DailyNode {
        anode: None,
        bnode: None,
        cnode: None,
        dnode: None,
    };

    for (day_num, day) in master_vec.into_iter().enumerate() {
        let mut day_vec: Vec<(Vec<usize>, (Option<Class>, Option<Class>))> = Vec::new();
        for basicpath in day {
            let path = pathfinding::time_path(basicpath.start, basicpath.end, &mut nodes);
            match basicpath.start_spot {
                Location::Classroom(start_class) => match basicpath.end_spot {
                    Location::Classroom(end_class) => {
                        day_vec.push((path, (Some(start_class), Some(end_class))))
                    }
                    _ => day_vec.push((path, (Some(start_class), None))),
                },
                _ => match basicpath.end_spot {
                    Location::Classroom(end_class) => day_vec.push((path, (None, Some(end_class)))),
                    _ => day_vec.push((path, (None, None))),
                },
            }
        }

        match day_num {
            0 => daily_node.anode = Some(day_vec),
            1 => daily_node.bnode = Some(day_vec),

            2 => daily_node.cnode = Some(day_vec),
            3 => daily_node.dnode = Some(day_vec),
            _ => return Err(format!("Unexpected day_num: {day_num}")),
        }
    }

    Ok((daily_node, nodes))
}
