use bevy::prelude::*;
use bevy::reflect::TypeUuid;
use std::any::Any;
use bevy::utils::HashMap;
use bevy::ecs::system::Command;


/// Plugin that adds ability to write and execute [`Program`]s which can stitch together arbitrary actions in a sequence.
/// This allows behaviors like cutscenes, npc movement, etc.
pub struct GluePlugin;
impl Plugin for GluePlugin {
    fn build(&self, app: &mut App) {
        app
            .add_asset::<Program>()
            .add_system_to_stage(CoreStage::PreUpdate, update_program_instances);
    }
}

/// Resource that stores a list of [`Instruction`]s.
/// Acts as an asset.
#[derive(TypeUuid)]
#[uuid="4f285b86-1994-11ed-861d-0242ac120002"]
pub struct Program {
    instructions: Vec<InstructionData>,                 // Instructions stored with some metadata
    instruction_labels: HashMap<&'static str, usize>    // Labels for instructions. Used for jump instructions.
}

/// Used to create a [`Program`].
#[derive(Default)]
pub struct ProgramBuilder {
    instructions: Vec<InstructionData>,
    instruction_labels: HashMap<&'static str, usize>
}
impl ProgramBuilder {

    /// Labels next instruction. Useful for use in jump instructions.
    pub fn label(&mut self, label: &'static str) {
        self.instruction_labels.insert(label, self.instructions.len());
    }

    /// Adds a sync instruction that will halt the [`ProgramInstance`] until finished.
    /// Result is thrown away.
    pub fn syn(&mut self, instruction: impl Instruction) {
        let data = InstructionData::Sync(Box::new(instruction));
        self.instructions.push(data);
    }

    /// Adds a sync instruction that will halt the [`ProgramInstance`] until finished.
    /// Result is stored in the specied variable.
    pub fn syn_var(&mut self, var_label: &'static str, instruction: impl Instruction) {
        let data = InstructionData::SyncVar(Box::new(instruction), var_label);
        self.instructions.push(data);
    }

    /// Adds an async instruction that will not halt the [`ProgramInstance`].
    /// Result is thrown away.
    pub fn asyn(&mut self, instruction: impl Instruction) {
        let data = InstructionData::Async(Box::new(instruction));
        self.instructions.push(data);
    }

    /// Adds an async instruction that will not halt the [`ProgramInstance`].
    /// Result is stored in the specied variable and should be waited on before use.
    pub fn asyn_var(&mut self, var_label: &'static str, instruction: impl Instruction) {
        let data = InstructionData::AsyncVar(Box::new(instruction), var_label);
        self.instructions.push(data);
    }

    /// Builds final [`Program`]
    pub fn build(self) -> Program {
        Program {
            instructions: self.instructions,
            instruction_labels: self.instruction_labels
        }
    }
}

/// [`Component`] that represents the execution of a [`Program`].
#[derive(Component)]
pub struct ProgramInstance {
    program: Handle<Program>,           // Program being executed.
    pc: usize,                          // Program counter.
    subject: Option<Entity>,            // Entity considered the subject, which will be passed to the instructions.
    variables: HashMap<VarLabel, Var>,  // Variables that are either ready to be used, or pending.
    variables_waiting: Vec<VarLabel>    // Variables that are being waited on. Until this is empty, program will not advance.
}

impl ProgramInstance {
    /// Creates new instance
    pub fn new(program: Handle<Program>) -> Self {
        Self {
            program,
            pc: 0,
            subject: None,
            variables: HashMap::new(),
            variables_waiting: Vec::new()
        }
    }

    /// Advances the program instance.
    /// Returns true if program is finished.
    /// 
    /// * `program` - Program being executed.
    /// * `instance_entity` - Subject that is used if not explicitly set by the program.
    /// * `commands` - [`Commands`] used to manipulate the [`World`].
    pub fn advance(
        &mut self,
        program: &Program,
        instance_entity: Entity,
        commands: &mut Commands
    ) -> bool {

        loop {

            // Quits early if there are varaibles being waited on.
            if !self.variables_waiting.is_empty() {
                return false;
            }

            // Determines subject
            let subject = match self.subject {
                Some(subject) => subject,
                None => instance_entity
            };

            // Executes next instruction
            let ins_data = &program.instructions[self.pc];
            let ctx = match ins_data {
                InstructionData::Sync(ins) => {
                    let mut ctx = ProgramContext::new(
                        commands,
                        subject,
                        VarRef::Value(instance_entity, VarLabel::Last)
                    );
                    ins.run(&mut ctx);
                    self.variables.insert(VarLabel::Last, Var::Pending);
                    self.variables_waiting.push(VarLabel::Last);
                    ctx
                }
                InstructionData::SyncVar(ins, label_str) => {
                    let mut ctx = ProgramContext::new(
                        commands,
                        subject,
                        VarRef::Value(instance_entity, VarLabel::Last)
                    );
                    ins.run(&mut ctx);
                    let label = VarLabel::Str(label_str);
                    self.variables.insert(label, Var::Pending);
                    self.variables_waiting.push(label);
                    ctx
                }
                InstructionData::Async(ins) => {
                    let mut ctx = ProgramContext::new(
                        commands,
                        subject,
                        VarRef::Null
                    );
                    ins.run(&mut ctx);
                    ctx
                }
                InstructionData::AsyncVar(ins, label_str) => {
                    let label = VarLabel::Str(label_str);
                    let mut ctx = ProgramContext::new(
                        commands,
                        subject,
                        VarRef::Value(instance_entity, label)
                    );
                    ins.run(&mut ctx);
                    self.variables.insert(label, Var::Pending);
                    ctx
                }
            };

            // Updates program counter
            self.pc = match ctx.next_instruction {
                Some(str) => program.instruction_labels[str],
                None => self.pc + 1
            };

            // Quits early if program is finished
            if self.pc >= program.instructions.len() || ctx.terminate {
                return true;
            }

            // Updates subject using resulting ctx
            match ctx.next_subject {
                NextSubject::Entity(entity) => self.subject = Some(entity),
                NextSubject::Script => self.subject = None,
                NextSubject::Same => {}
            }

            // Handles immediate result of instruction, if there was one
            match (ctx.var_ref, ctx.var_result) {
                (VarRef::Null, _) => return false,
                (VarRef::Value(..), None) => return false,
                (VarRef::Value(.., label), Some(result)) => {
                    self.variables.insert(label, Var::Value(result));
                }
            }
        }
    }

    /// Completes variable if one already existed in the pending state
    fn complete_var(&mut self, var_label: VarLabel, value: impl Any + Send + Sync + 'static) {
        let var = match self.variables.get_mut(&var_label) {
            Some(var) => var,
            None => return
        };
        *var = match var {
            Var::Pending => Var::Value(Box::new(value)),
            Var::Value(_) => return
        };
    }
}

/// Context that an [`Instruction`] uses to execute.
pub struct ProgramContext<'c, 'w, 's> {
    /// World to manipulate
    pub commands: &'c mut Commands<'w, 's>,
    /// Entity considered the "subject"
    pub subject: Entity,
    /// Reference to variable to be completed at a later time
    pub var_ref: VarRef,
    /// Entity that should be considered the subject for subsequent instructions
    next_subject: NextSubject,
    /// Instruction label to jump to after this instruction finishes
    next_instruction: Option<&'static str>,
    /// Result of var_ref. Should be set by an "immediate" instruction
    var_result: Option<Box<dyn Any + Send + Sync + 'static>>,
    /// Instructions program to terminate if true
    terminate: bool
}
impl<'c, 'w, 's> ProgramContext<'c, 'w, 's> {
    pub fn new(
        commands: &'c mut Commands<'w, 's>,
        subject: Entity,
        var_ref: VarRef
    ) -> Self {
        Self {
            commands,
            subject,
            var_ref,
            next_subject: NextSubject::Same,
            next_instruction: None,
            var_result: None,
            terminate: false
        }
    }

    /// Overrides subject for future instructions
    pub fn set_subject(&mut self, entity: Entity) {
        self.next_subject = NextSubject::Entity(entity);
    }

    /// Sets subject to script entity for future instructions
    pub fn reset_subject(&mut self) {
        self.next_subject = NextSubject::Script;
    }

    /// Sets labeled instruction to jump to after instruction is finished
    pub fn jump_to(&mut self, instruction_label: &'static str) {
        self.next_instruction = Some(instruction_label)
    }

    /// Marks current instruction as finished. Useful for "immediate" instructions
    pub fn finish(&mut self, var_result: impl Any + Send + Sync + 'static) {
        self.var_result = Some(Box::new(var_result));
    }

    /// Forces program to terminate if invoked
    pub fn termiante(&mut self) {
        self.terminate = true;
    }
}

/// Command to set the subject [`Entity`].
enum NextSubject {
    Same,           // Same subject as current subject
    Entity(Entity), // Set subject to specific entity
    Script          // Set subject to script entity
}


/// Instruction to be run in a [`ProgramInstance`]
pub trait Instruction: Send + Sync + 'static {
    /// Runs the instruction
    fn run(&self, commands: &mut ProgramContext);
}

/// Variable set in a [`ProgramInstance`].
/// Can either be pending, or have an explicit value.
pub enum Var {
    Pending,
    Value(Box<dyn Any + Send + Sync + 'static>)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum VarLabel {
    /// Label that names the result of the last instruction run.
    Last,
    /// Label that names the result of an arbitrary instruction.
    Str(&'static str)
}

/// Component that references a variable in a [`ProgramInstance`].
/// Meant to be "completed" for later use in the instance.
#[derive(Component)]
pub enum VarRef { 
    Value(Entity, VarLabel),
    Null
}

/// Instruction bundled with some extra information about how it should be run.
pub enum InstructionData {
    /// Instruction that should be executed and waited on. Result is thrown away.
    Sync(Box<dyn Instruction>),
    /// Instruction that should be executed and waited on. Result is stored in specified variable and ready for immediate use.
    SyncVar(Box<dyn Instruction>, &'static str),
    /// Instruction that should be executed and not waited on. Result is thrown away.
    Async(Box<dyn Instruction>),
    /// Instruction that should be executed and not waited on. Result is stored in specified variable that should be waited on before use.
    AsyncVar(Box<dyn Instruction>, &'static str)
}


/// Event to be fired that finishes
pub struct FinishVarEvent {
    pub entity: Entity,
    pub result: Box<dyn Any + Send + Sync + 'static>
}
impl FinishVarEvent {
    pub fn new(entity: Entity, result: impl Any + Send + Sync + 'static) -> Self {
        Self {
            entity,
            result: Box::new(result)
        }
    }
}

/// Extends [`Commands`] such that [`ProgramCommands`] can be used
pub trait CommandsExt<'w, 's, 'a> {
    fn program(&'a mut self) -> ProgramCommands<'w, 's, 'a>;
}
impl<'w, 's, 'a> CommandsExt<'w, 's, 'a> for Commands<'w, 's> {
    fn program(&'a mut self) -> ProgramCommands<'w, 's, 'a> {
        ProgramCommands {
            commands: self
        }
    }
}

/// Writes commands pertaining to a [`ProgramInstance`]
pub struct ProgramCommands<'w, 's, 'a> {
    commands: &'a mut Commands<'w, 's>
}
impl<'w, 's, 'a> ProgramCommands<'w, 's, 'a> {
    pub fn complete_var(&mut self, var_ref: VarRef, value: impl Any + Send + Sync + 'static) {
        self.commands.add(CompleteVar {
            var_ref,
            value: Box::new(value)
        })
    }
}

/// Command to complete a pending variable in a [`ProgramInstance`].
pub struct CompleteVar {
    var_ref: VarRef,
    value: Box<dyn Any + Send + Sync + 'static>
}
impl Command for CompleteVar {
    fn write(self, world: &mut World) {

        // Unpacks variable reference
        let (entity, label) = match self.var_ref {
            VarRef::Value(entity, label) => (entity, label),
            VarRef::Null => return
        };

        // Fetches program instance
        let mut eref = match world.get_entity_mut(entity) {
            Some(eref) => eref,
            None => return
        };
        let mut program_instance = match eref.get_mut::<ProgramInstance>() {
            Some(instance) => instance,
            None => return
        };

        // Completes variable
        program_instance.complete_var(label, self.value);
    }
}

/// Advances non-halted program instances
fn update_program_instances(
    mut commands: Commands,
    mut instances: Query<(Entity, &mut ProgramInstance)>,
    programs: Res<Assets<Program>>
) {
    for (entity, mut instance) in &mut instances {
        let program = match programs.get(&instance.program) {
            Some(program) => program,
            None => continue
        };
        instance.advance(&program, entity, &mut commands);
    }
}