/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2017                              *
 *  Author: Ulrich Kremer                    *
 *  Student Version                          *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

static int numOutputVariables = 0, *outputRegisters;

int getRegStored(Instruction *head, int offset, int reg){
	Instruction *instr = head;
	do{
		if(instr->opcode == STOREAI){
			if(instr->field2 == reg && instr->field3 == offset) return instr->field1;
		}
		instr = instr->prev;
	}while(instr != NULL);
	return -1;
}

void setRegisterIsOutput(int reg){
	numOutputVariables ++;
	if(numOutputVariables == 1){
		outputRegisters = malloc(sizeof(int) * numOutputVariables);
	}else{
		outputRegisters = realloc(outputRegisters, sizeof(int) * numOutputVariables);
	}
	outputRegisters[numOutputVariables - 1] = reg;
}

int isRegisterOutput(int reg){
	int i;
	for(i = 0; i < numOutputVariables; i ++){
		if(reg == outputRegisters[i]) return 1;
	}
	return 0;
}

void removeInstruction(Instruction *instr){
	if(!instr) return; // A NULL INSTR! BAD BOY!
	if(instr->prev != NULL){
		instr->prev->next = instr->next; // Set the previous instruction's next to this instruction's next.
	}
	if(instr->next != NULL){
		instr->next->prev = instr->prev; // Set the next instruction's next to this instruction's previous.
	}
	free(instr);
}

int main(){
	Instruction *head, *instr, *instr2;
	int reg1, reg2;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

	instr = head;

	setRegisterIsOutput(0);

	// Forward pass to get all registers that are direct output:
	while(instr != NULL){
		if(instr->opcode == OUTPUTAI){
			reg1 = getRegStored(instr, instr->field2, instr->field1);
			setRegisterIsOutput(reg1);
		}
		instr2 = instr;	// Preserves the end node.
		instr = instr->next;
	}

	// Now we perform a backwards pass to find all things which contribute to output.
	instr = instr2;	// Preserve the actual last node before we got to nonexistant one.
	while(instr != NULL){
		if(instr->opcode == ADD || instr->opcode == SUB ||
			instr->opcode == MUL || instr->opcode == DIV){
			if(isRegisterOutput(instr->field3)){
				// If this register is used in output, then both registers in this operation are, too.
				setRegisterIsOutput(instr->field1);
				setRegisterIsOutput(instr->field2);
			}
		}else if(instr->opcode == LOADAI){
			if(isRegisterOutput(instr->field3)){
				// We are loading a value which is important for the output, so whatever is stored there is important.
				reg1 = getRegStored(instr, instr->field2, instr->field1);
				setRegisterIsOutput(reg1);
			}
		}
		instr = instr->prev;
	}

	instr = head;
	while(instr != NULL){
		instr2 = NULL;
		if(instr->opcode == ADD || instr->opcode == SUB ||
                        instr->opcode == MUL || instr->opcode == DIV){
			reg1 = instr->field3;
			if(!isRegisterOutput(reg1)) instr2 = instr;
		}else if(instr->opcode == LOADI){
			reg1 = instr->field2;
			if(!isRegisterOutput(reg1)) instr2 = instr;
		}else if(instr->opcode == LOADAI){
			reg1 = instr->field3;
			if(!isRegisterOutput(reg1)) instr2 = instr;
		}else if(instr->opcode == STOREAI){
			reg1 = instr->field1;
			if(!isRegisterOutput(reg1)) instr2 = instr;
		}
		instr = instr->next;
		if(instr2 != NULL) removeInstruction(instr2);
	}

	if (head)
		PrintInstructionList(stdout, head);

	return EXIT_SUCCESS;
}

