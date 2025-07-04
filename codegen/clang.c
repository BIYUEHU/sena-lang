#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>

// ==================== 运行时系统 ====================

// 错误处理
typedef struct
{
  char *message;
} MihamaError;

void mihama_panic(const char *msg)
{
  fprintf(stderr, "MihamaError: %s\n", msg);
  exit(1);
}

// 类型系统
typedef enum
{
  MIHAMA_KIND,
  MIHAMA_STRING,
  MIHAMA_INT,
  MIHAMA_BOOL,
  MIHAMA_CHAR,
  MIHAMA_FLOAT
} MihamaTypeTag;

typedef struct
{
  MihamaTypeTag tag;
  char *name;
} MihamaType;

// 值的统一表示
typedef struct MihamaValue
{
  char *value_tag;
  void *value;
  size_t ref_count;
} MihamaValue;

// 引用计数管理
MihamaValue *mihama_retain(MihamaValue *val)
{
  if (val)
    val->ref_count++;
  return val;
}

void mihama_release(MihamaValue *val)
{
  if (!val)
    return;
  val->ref_count--;
  if (val->ref_count == 0)
  {
    if (val->value)
      free(val->value);
    if (val->value_tag)
      free(val->value_tag);
    free(val);
  }
}

// 基础类型构造器
MihamaType *mihama_type_new(const char *name)
{
  MihamaType *type = malloc(sizeof(MihamaType));
  type->name = strdup(name);
  return type;
}

MihamaValue *mihama_value_new(const char *tag, void *value)
{
  MihamaValue *val = malloc(sizeof(MihamaValue));
  val->value_tag = strdup(tag);
  val->value = value;
  val->ref_count = 1;
  return val;
}

MihamaValue *mihama_int(int64_t i)
{
  int64_t *val = malloc(sizeof(int64_t));
  *val = i;
  return mihama_value_new("Int", val);
}

MihamaValue *mihama_float(double f)
{
  double *val = malloc(sizeof(double));
  *val = f;
  return mihama_value_new("Float", val);
}

MihamaValue *mihama_string(const char *s)
{
  char *val = strdup(s);
  return mihama_value_new("String", val);
}

MihamaValue *mihama_char(char c)
{
  char *val = malloc(sizeof(char));
  *val = c;
  return mihama_value_new("Char", val);
}

// 类型比较
bool mihama_equal(MihamaValue *a, MihamaValue *b)
{
  if (!a || !b)
    return false;
  if (strcmp(a->value_tag, b->value_tag) != 0)
    return false;

  if (strcmp(a->value_tag, "Int") == 0)
  {
    return *(int64_t *)a->value == *(int64_t *)b->value;
  }
  else if (strcmp(a->value_tag, "String") == 0)
  {
    return strcmp((char *)a->value, (char *)b->value) == 0;
  }
  else if (strcmp(a->value_tag, "Char") == 0)
  {
    return *(char *)a->value == *(char *)b->value;
  }
  else if (strcmp(a->value_tag, "Float") == 0)
  {
    return *(double *)a->value == *(double *)b->value;
  }
  return a == b; // 对于其他类型，比较指针
}

// 柯里化函数支持
typedef struct MihamaCurried
{
  void *func_ptr;
  int expected_args;
  int current_args;
  MihamaValue **captured_args;
} MihamaCurried;

MihamaCurried *mihama_curry_new(void *func, int arity)
{
  MihamaCurried *curried = malloc(sizeof(MihamaCurried));
  curried->func_ptr = func;
  curried->expected_args = arity;
  curried->current_args = 0;
  curried->captured_args = malloc(sizeof(MihamaValue *) * arity);
  return curried;
}

// 全局变量声明
MihamaType *MihamaKind;
MihamaType *MihamaString;
MihamaType *MihamaInt;
MihamaType *MihamaBool;
MihamaType *MihamaChar;
MihamaType *MihamaFloat;

// 预定义函数
void mihama_print(MihamaValue *val)
{
  if (!val)
  {
    printf("null\n");
    return;
  }

  if (strcmp(val->value_tag, "Int") == 0)
  {
    printf("%lld\n", *(int64_t *)val->value);
  }
  else if (strcmp(val->value_tag, "String") == 0)
  {
    printf("%s\n", (char *)val->value);
  }
  else if (strcmp(val->value_tag, "Char") == 0)
  {
    printf("%c\n", *(char *)val->value);
  }
  else if (strcmp(val->value_tag, "Float") == 0)
  {
    printf("%f\n", *(double *)val->value);
  }
  else
  {
    printf("[%s]\n", val->value_tag);
  }
}

double mihama_get_timestamp()
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return ts.tv_sec + ts.tv_nsec / 1000000000.0;
}

MihamaValue *mihama_concat(MihamaValue *a, MihamaValue *b)
{
  if (!a || !b)
    return NULL;
  char *str_a = (char *)a->value;
  char *str_b = (char *)b->value;
  size_t len = strlen(str_a) + strlen(str_b) + 1;
  char *result = malloc(len);
  snprintf(result, len, "%s%s", str_a, str_b);
  return mihama_value_new("String", result);
}

// ==================== 转译代码 ====================

// 前置声明
MihamaValue *add_impl(MihamaValue *x, MihamaValue *y);
MihamaValue *add1_impl(MihamaValue *x, MihamaValue *y);
MihamaValue *fibonacii(MihamaValue *x);
MihamaValue *toNumber(MihamaValue *x);
MihamaValue *Cons_impl(MihamaValue *arg0, MihamaValue *arg1);
MihamaValue *S_impl(MihamaValue *arg0);
MihamaValue *C_impl(MihamaValue *el, MihamaValue *list);
MihamaValue *O_impl(MihamaValue *f, MihamaValue *x);
MihamaValue *T_impl(MihamaValue *g, MihamaValue *f);
MihamaValue *handleScore(MihamaValue *x);

// 柯里化包装器
typedef MihamaValue *(*BinaryFunc)(MihamaValue *, MihamaValue *);
typedef MihamaValue *(*UnaryFunc)(MihamaValue *);

MihamaValue *curry2_apply(BinaryFunc func, MihamaValue *arg1)
{
  // 返回部分应用的函数（这里简化处理）
  return func(arg1, NULL); // 实际需要更复杂的闭包机制
}

// 全局变量
MihamaValue *foo;
MihamaValue *foo1;
MihamaValue *foo2;
MihamaType *Bool2;
MihamaValue *True;
MihamaValue *False;
MihamaValue *foo3;
MihamaValue *foo4;
MihamaType *List;
MihamaValue *Cons;
MihamaValue *Nil;
MihamaType *String1;
MihamaValue *fruits;
MihamaValue *add_curried;
MihamaValue *add1_curried;
MihamaType *Nat;
MihamaValue *Z;
MihamaValue *S;
MihamaValue *num;
MihamaValue *bar;
MihamaValue *bar2;
MihamaValue *C;
MihamaValue *O;
MihamaValue *T;

// 实现函数
MihamaValue *add_impl(MihamaValue *x, MihamaValue *y)
{
  if (!x || !y)
    return NULL;
  int64_t val_x = *(int64_t *)x->value;
  int64_t val_y = *(int64_t *)y->value;
  return mihama_int(val_x + val_y);
}

MihamaValue *add1_impl(MihamaValue *x, MihamaValue *y)
{
  return add_impl(x, y);
}

MihamaValue *fibonacii(MihamaValue *x)
{
  if (!x)
    return NULL;
  int64_t val = *(int64_t *)x->value;

  if (val == 1)
  {
    return mihama_int(1);
  }
  else if (val == 2)
  {
    return mihama_int(1);
  }
  else
  {
    MihamaValue *x_minus_1 = mihama_int(val - 1);
    MihamaValue *x_minus_2 = mihama_int(val - 2);
    MihamaValue *fib1 = fibonacii(x_minus_1);
    MihamaValue *fib2 = fibonacii(x_minus_2);
    MihamaValue *result = add_impl(fib1, fib2);

    mihama_release(x_minus_1);
    mihama_release(x_minus_2);
    mihama_release(fib1);
    mihama_release(fib2);

    return result;
  }
}

MihamaValue *Cons_impl(MihamaValue *arg0, MihamaValue *arg1)
{
  MihamaValue **cons_data = malloc(sizeof(MihamaValue *) * 2);
  cons_data[0] = mihama_retain(arg0);
  cons_data[1] = mihama_retain(arg1);
  return mihama_value_new("Cons", cons_data);
}

MihamaValue *S_impl(MihamaValue *arg0)
{
  MihamaValue **s_data = malloc(sizeof(MihamaValue *));
  s_data[0] = mihama_retain(arg0);
  return mihama_value_new("S", s_data);
}

MihamaValue *toNumber(MihamaValue *x)
{
  if (!x)
    return mihama_int(0);

  if (strcmp(x->value_tag, "S") == 0)
  {
    MihamaValue *n = ((MihamaValue **)x->value)[0];
    if (mihama_equal(n, Z))
    {
      return mihama_int(1);
    }
    else
    {
      MihamaValue *sub_result = toNumber(n);
      MihamaValue *one = mihama_int(1);
      MihamaValue *result = add_impl(one, sub_result);
      mihama_release(sub_result);
      mihama_release(one);
      return result;
    }
  }
  else if (strcmp(x->value_tag, "Z") == 0)
  {
    return mihama_int(0);
  }
  else
  {
    mihama_panic("Match pattern not exhaustive");
    return NULL;
  }
}

MihamaValue *C_impl(MihamaValue *el, MihamaValue *list)
{
  return Cons_impl(el, list);
}

MihamaValue *O_impl(MihamaValue *f, MihamaValue *x)
{
  // 简化的函数应用，实际需要更复杂的机制
  return NULL; // 需要实现函数调用机制
}

MihamaValue *T_impl(MihamaValue *g, MihamaValue *f)
{
  // 函数组合，简化处理
  return NULL; // 需要实现函数组合机制
}

MihamaValue *handleScore(MihamaValue *x)
{
  if (!x)
    return mihama_string("Fake");
  int64_t score = *(int64_t *)x->value;

  if (score > 100 || score < 0)
  {
    return mihama_string("Fake");
  }
  else if (score == 100)
  {
    return mihama_string("Best");
  }
  else if (score > 90)
  {
    return mihama_string("Excellent");
  }
  else if (score > 70)
  {
    return mihama_string("Good");
  }
  else if (score > 60)
  {
    return mihama_string("Not bad");
  }
  else if (score > 40)
  {
    return mihama_string("Bad");
  }
  else
  {
    return mihama_string("Shit");
  }
}

void mihama_init()
{
  MihamaKind = mihama_type_new("Kind");
  MihamaString = mihama_type_new("String");
  MihamaInt = mihama_type_new("Int");
  MihamaBool = mihama_type_new("Bool");
  MihamaChar = mihama_type_new("Char");
  MihamaFloat = mihama_type_new("Float");
}

// 主程序
void mihama_main()
{
  // let foo = 1
  foo = mihama_int(1);

  // let foo1 = "Hello, FP!"
  foo1 = mihama_string("Hello, FP!");

  // let foo2: Char = 'X'
  foo2 = mihama_char('X');

  // type Bool2 = True | False
  Bool2 = mihama_type_new("Bool2");
  True = mihama_value_new("True", NULL);
  False = mihama_value_new("False", NULL);

  // let foo3: Bool2 = False
  foo3 = mihama_retain(False);

  // let foo4: Float = 3.1415926
  foo4 = mihama_float(3.1415926);

  // type List: Kind -> Kind = <T> Cons(T, List(T)) | Nil
  List = mihama_type_new("List");
  Nil = mihama_value_new("Nil", NULL);

  // let String1 = List(Char)
  String1 = mihama_type_new("List<Char>");

  // let fruits: List = Cons("Apple", Cons("Banana", Cons("Pear", Nil)))
  fruits = Cons_impl(mihama_string("Apple"),
                     Cons_impl(mihama_string("Banana"),
                               Cons_impl(mihama_string("Pear"), Nil)));

  // type Nat = Z | S(Nat)
  Nat = mihama_type_new("Nat");
  Z = mihama_value_new("Z", NULL);

  // let num = S(S(S(Z)))
  num = S_impl(S_impl(S_impl(Z)));

  // print(add(toNumber(num), 3))
  MihamaValue *num_value = toNumber(num);
  MihamaValue *three = mihama_int(3);
  MihamaValue *result = add_impl(num_value, three);
  mihama_print(result);

  // let bar = let x = 20 in let y = 30 in x + y ^ 2 * 2 / 5 - 5 % 1
  int64_t x = 20;
  int64_t y = 30;
  int64_t bar_val = x + (int64_t)pow(y, 2) * 2 / 5 - 5 % 1;
  bar = mihama_int(bar_val);

  // let bar2 = { let x = 20; let y = 30; x + y ^ 2 * 2 / 5 - 5 % 1 }
  bar2 = mihama_int(bar_val); // 同样的计算

  // 清理资源
  mihama_release(num_value);
  mihama_release(three);
  mihama_release(result);
}

int main()
{
  mihama_init();
  mihama_main();

  // 这里应该有更完善的资源清理
  return 0;
}