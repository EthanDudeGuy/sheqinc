#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//what we have:
//-Values 
//-expression
//-bindings
//-env
//^^^ these are structs an unions
//
//Next time:
//-top env
//-start on interp
//
//
//DEFINITIONS SO COMPILER ISNT MAD
struct idC;
struct numC;
struct stringC;
struct ifC;
struct lambdaC;
struct appC;

struct numV;
struct stringV;
struct boolV;
struct cloV;
struct primV;
struct nullV;


//EXPRS
typedef enum {
	EXPR_IDC,
	EXPR_NUMC,
	EXPR_STRINGC,
	EXPR_IFC,
	EXPR_LAMBC,
	EXPR_APPC,
} ExprTag;

typedef struct Expr {
	ExprTag tag;
	union {
		struct idC *idC;
		struct numC *numC;
		struct stringC *stringC;
		struct ifC *ifC;
		struct lambdaC *lambdaC;
		struct appC *appC;
	} u;
} Expr;
//EXPRS

//EXPR STRUCT DEFS
struct idC { char *name; };
struct numC { float num; };
struct stringC { char *string; };
struct ifC {
	Expr *test;
	Expr *trueval;  
	Expr *falseval;  
};

//helper for lambda  and app
struct arglist {
	int len;
	char **args;
};	
//end helper

//appc helper struct
struct exprlist {
	int len;
	Expr *args;
};
//end helper

struct lambdaC {
	struct arglist arglist;
	Expr *body;
};
struct appC {
	Expr *function;
	struct exprlist args;
};
//EXPR STRUCT DEFS


//VALS
typedef enum {
	VAL_NUMBER,
	VAL_STRING,
	VAL_BOOLEAN,
	VAL_CLOSURE,
	VAL_PRIMITIVE,
	VAL_NULL,
} valTag;

typedef struct val {
	valTag tag;
	union {
		struct numV *numV;
		struct stringV *stringV;
		struct boolV *boolV;
		struct cloV *cloV;
		struct primV *primV;
		struct nullV *nullV; 
	} u;
} val;
//VALS


//ENVIRONMENTS
typedef struct binding {
	char *name;
	val value;
} binding;


typedef struct Environment {
	int len;
	binding *bindings;
} Environment;
//ENVIRONMENTS

//VAL STRUCT DEFS
struct numV { float num; };
struct stringV { char *string; };
struct boolV { int b; };
struct cloV {
	struct arglist params;
	Expr *body;
	Environment env; 
};
struct nullV { };
//VAL STRUCT DEFS



val lookup_in_env(Environment env, char* name) {
	val retval;
	for (int i=0; i < env.len; i++) {

		if (strcmp((char*)(env.bindings[i].name), name) == 0) {
			return env.bindings[i].value;
		}
	}
	printf("SHEQ: unbound in env %s\n", name);
	exit(1);
}



struct primV { struct idC op; };

val interp(Expr exp, Environment env) {
	val retval;

	switch(exp.tag){
		case EXPR_IDC:
			retval = lookup_in_env(env, exp.u.idC->name);
			if (!(retval.tag == VAL_NULL)) {
				return retval;
			} else {
				exit(1);
			}
			break;
		case EXPR_NUMC:
			retval = (val){
				.tag = VAL_NUMBER,
				.u.numV = malloc(sizeof(struct numV))
			};
			retval.u.numV->num = (float)(exp.u.numC->num);
			return retval;
		case EXPR_STRINGC:
			printf("%s\n", exp.u.stringC->string);
			retval = (val){
				.tag = VAL_STRING,
				.u.stringV = malloc(sizeof(struct stringV))
			};
			retval.u.stringV->string = (char *)(exp.u.stringC->string);
			return retval;
		case EXPR_IFC:
			val valboolfromcheck;
			valboolfromcheck = interp(*(exp.u.ifC->test), env);
			if (valboolfromcheck.tag == VAL_BOOLEAN) {
				printf("%d\n", valboolfromcheck.u.boolV->b);
				if (valboolfromcheck.u.boolV->b == 1) {
					retval = interp(*(exp.u.ifC->trueval), env);
					printf("%f\n", retval.u.numV->num);
					return retval;
				} else {
					retval = interp(*(exp.u.ifC->falseval), env);
					return retval;
				}
			} else {
				printf("SHEQ: conditional check must be a boolean\n");
				exit(1);	
			}
		case EXPR_LAMBC:
			retval = (val){
				.tag = VAL_CLOSURE,
				.u.cloV = malloc(sizeof(struct cloV))
			};
			retval.u.cloV->params = exp.u.lambdaC->arglist;	
			retval.u.cloV->body = exp.u.lambdaC->body;
			retval.u.cloV->env = env;
			return retval;
		case EXPR_APPC:
			//this is the function closure *(exp.u.function)
			Environment new_env;
			struct binding newbinding;
			int currentenvlen = env.len;
		        val func = interp(*(exp.u.appC->function), env);
			if (func.tag != VAL_CLOSURE) {
				printf("SHEQ: application of nonfunction\n");
				exit(1);
			}
			if (func.u.cloV->params.len != exp.u.appC->args.len) {
				printf("SHEQ: args and params len mismatch\n");
				exit(1);
			}
			for (int i = 0; i < func.u.cloV->params.len; i++) {
				newbinding = (binding){
					.name = func.u.cloV->params.args[i],
					.value = interp(exp.u.appC->args.args[i], env)
				};
				new_env = (Environment){
					.len = currentenvlen + 1,
					.bindings = realloc(env.bindings, sizeof(struct binding) * (currentenvlen + 1))
				};
				new_env.bindings[new_env.len] = newbinding;
				currentenvlen = new_env.len;
			}
			retval = interp(*(func.u.cloV->body), new_env);
			return retval;
		default:
			printf("SHEQ: invalid AST\n");
			exit(1);	
	}
}



int main() {
	//minimum top environment
	Environment top_env;
	val interpretval;
	val retfromenv;
	
	//instantiate env
	top_env.len=2;
	top_env.bindings = (binding*)malloc(sizeof(binding) * 11);

	//true prim
	top_env.bindings[0] = (binding){
		.name = "true",
		.value = {
			.tag = VAL_BOOLEAN,
			.u.boolV = malloc(sizeof(struct boolV))
		}
	};
	top_env.bindings[0].value.u.boolV->b = 1;

	//false prim
	top_env.bindings[1] = (binding){
		.name = "false",
		.value = {
			.tag = VAL_BOOLEAN,
			.u.boolV = malloc(sizeof(struct boolV))
		}
	};
	top_env.bindings[1].value.u.boolV->b = 0;
	//minimum top environment

	/////////////////////////////////////////////////////////////
	//TESTS
	/////////////////////////////////////////////////////////////

	//INTERP OF A NUM
	//Expr examplenum = (Expr){
	//			.tag = EXPR_NUMC,
	//			.u.numC = malloc(sizeof(struct numC))
	//			};
	//examplenum.u.numC->num = (float)5;
	//
	//interpretval = interp(examplenum, top_env);


	//ENV LOOKUP
	//retfromenv = lookup_in_env(top_env, "true");
	//printf("%d\n", retfromenv.u.boolV->b);
	
	
	
	//INTERP OF A STRING
	//Expr examplestring = (Expr){
	//			.tag = EXPR_STRINGC,
	//			.u.stringC = malloc(sizeof(struct stringC))
	//			};
	//examplestring.u.stringC->string = "hello";
	//
	//interpretval = interp(examplestring, top_env);
	//printf("%s\n", interpretval.u.stringV->string);
	

	//INTERP OF A VAR
	//Expr examplevar = (Expr){
	//			.tag = EXPR_IDC,
	//			.u.idC = malloc(sizeof(struct idC))
	//			};
	//examplevar.u.idC->name = "true";
	//
	//interpretval = interp(examplevar, top_env);
	//printf("%d\n", interpretval.u.boolV->b);


	//INTERP OF A CONDITIONAL
	//Expr exampleif = (Expr){
	//			.tag = EXPR_IFC,
	//			.u.ifC = malloc(sizeof(struct ifC))
	//			};
	//
	//Expr iftest = (Expr){
	//			.tag = EXPR_IDC,
	//			.u.idC = malloc(sizeof(struct idC))
	//			};
	//iftest.u.idC->name = "false";
	//
	Expr examplenum = (Expr){
				.tag = EXPR_NUMC,
				.u.numC = malloc(sizeof(struct numC))
				};
	examplenum.u.numC->num = (float)5;
	
	Expr examplenumfalse = (Expr){
				.tag = EXPR_NUMC,
				.u.numC = malloc(sizeof(struct numC))
				};
	examplenumfalse.u.numC->num = (float)6;
	//
	//exampleif.u.ifC->test = &iftest;
	//exampleif.u.ifC->trueval = &examplenum;
	//exampleif.u.ifC->falseval = &examplenumfalse;
	//
	//interpretval = interp(exampleif, top_env);
	//
	//printf("%d\n", (interpretval.tag == VAL_NUMBER));
	//printf("%f\n", interpretval.u.numV->num);
	//
	char *args[] = {"x", "y"};
	struct arglist examplearglist = {
		.len = 2,
		.args = args
	};
	struct exprlist params = {
		.len = 2,
		.args = {&examplenum, &examplenumfalse}
	};
	
	
	Expr examplelamb = (Expr){
					.tag = EXPR_LAMBC,
					.u.lambdaC = malloc(sizeof(struct lambdaC))
				};
	examplelamb.u.lambdaC->arglist = examplearglist;
	examplelamb.u.lambdaC->body = &examplenumfalse;
	
	//interpretval = interp(examplelamb, top_env);
	
	//
	//printf("%d\n", (interpretval.tag == VAL_CLOSURE));
	//printf("%d\n", interpretval.u.cloV->params.len);
	Expr exampleapp = (Expr){
					.tag = EXPR_APPC,
					.u.appC = malloc(sizeof(struct appC))
				};
	exampleapp.u.appC->function = &examplelamb;
	exampleapp.u.appC->args = params;

	interpretval = interp(exampleapp, top_env);



}
