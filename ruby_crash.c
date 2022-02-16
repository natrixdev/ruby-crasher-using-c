   if (!jit_at_current_insn(jit)) {
        defer_compilation(jit, ctx);
        return YJIT_END_BLOCK;
    }

    VALUE comptime_a = jit_peek_at_stack(jit, ctx, 1);
    VALUE comptime_b = jit_peek_at_stack(jit, ctx, 0);

    if (FIXNUM_P(comptime_a) && FIXNUM_P(comptime_b)) {
        // Create a side-exit to fall back to the interpreter
        // Note: we generate the side-exit before popping operands from the stack
        uint8_t *side_exit = yjit_side_exit(jit, ctx);

        if (!assume_bop_not_redefined(jit, INTEGER_REDEFINED_OP_FLAG, BOP_PLUS)) {
            return YJIT_CANT_COMPILE;
        }

        // Check that both operands are fixnums
        guard_two_fixnums(ctx, side_exit);

        // Get the operands and destination from the stack
        x86opnd_t arg1 = ctx_stack_pop(ctx, 1);
        x86opnd_t arg0 = ctx_stack_pop(ctx, 1);

        // Add arg0 + arg1 and test for overflow
        mov(cb, REG0, arg0);
        sub(cb, REG0, imm_opnd(1));
        add(cb, REG0, arg1);
        jo_ptr(cb, side_exit);

        // Push the output on the stack
        x86opnd_t dst = ctx_stack_push(ctx, TYPE_FIXNUM);
        mov(cb, dst, REG0);

        return YJIT_KEEP_COMPILING;
    }
    else {
        // Delegate to send, call the method on the recv
        return gen_opt_send_without_block(jit, ctx, cb);
    }
}

static codegen_status_t
gen_opt_mult(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    // Delegate to send, call the method on the recv
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_div(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    // Delegate to send, call the method on the recv
    return gen_opt_send_without_block(jit, ctx, cb);
}

VALUE rb_vm_opt_mod(VALUE recv, VALUE obj);

static codegen_status_t
gen_opt_mod(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    // Save the PC and SP because the callee may allocate bignums
    // Note that this modifies REG_SP, which is why we do it first
    jit_prepare_routine_call(jit, ctx, REG0);

    uint8_t *side_exit = yjit_side_exit(jit, ctx);

    // Get the operands from the stack
    x86opnd_t arg1 = ctx_stack_pop(ctx, 1);
    x86opnd_t arg0 = ctx_stack_pop(ctx, 1);

    // Call rb_vm_opt_mod(VALUE recv, VALUE obj)
    mov(cb, C_ARG_REGS[0], arg0);
    mov(cb, C_ARG_REGS[1], arg1);
    call_ptr(cb, REG0, (void *)rb_vm_opt_mod);

    // If val == Qundef, bail to do a method call
    cmp(cb, RAX, imm_opnd(Qundef));
    je_ptr(cb, side_exit);

    // Push the return value onto the stack
    x86opnd_t stack_ret = ctx_stack_push(ctx, TYPE_UNKNOWN);
    mov(cb, stack_ret, RAX);

    return YJIT_KEEP_COMPILING;
}

static codegen_status_t
gen_opt_ltlt(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    // Delegate to send, call the method on the recv
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_nil_p(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    // Delegate to send, call the method on the recv
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_empty_p(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    // Delegate to send, call the method on the recv
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_str_freeze(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    if (!assume_bop_not_redefined(jit, STRING_REDEFINED_OP_FLAG, BOP_FREEZE)) {
        return YJIT_CANT_COMPILE;
    }

    VALUE str = jit_get_arg(jit, 0);
    jit_mov_gc_ptr(jit, cb, REG0, str);

    // Push the return value onto the stack
    x86opnd_t stack_ret = ctx_stack_push(ctx, TYPE_STRING);
    mov(cb, stack_ret, REG0);

    return YJIT_KEEP_COMPILING;
}

static codegen_status_t
gen_opt_str_uminus(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    if (!assume_bop_not_redefined(jit, STRING_REDEFINED_OP_FLAG, BOP_UMINUS)) {
        return YJIT_CANT_COMPILE;
    }

    VALUE str = jit_get_arg(jit, 0);
    jit_mov_gc_ptr(jit, cb, REG0, str);

    // Push the return value onto the stack
    x86opnd_t stack_ret = ctx_stack_push(ctx, TYPE_STRING);
    mov(cb, stack_ret, REG0);

    return YJIT_KEEP_COMPILING;
}

static codegen_status_t
gen_opt_not(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_size(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_length(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_regexpmatch2(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    return gen_opt_send_without_block(jit, ctx, cb);
}

static codegen_status_t
gen_opt_case_dispatch(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    // Normally this instruction would lookup the key in a hash and jump to an
    // offset based on that.
    // Instead we can take the fallback case and continue with the next
    // instruction.
    // We'd hope that our jitted code will be sufficiently fast without the
    // hash lookup, at least for small hashes, but it's worth revisiting this
    // assumption in the future.

    ctx_stack_pop(ctx, 1);

    return YJIT_KEEP_COMPILING; // continue with the next instruction
}

static void
gen_branchif_branch(codeblock_t *cb, uint8_t *target0, uint8_t *target1, uint8_t shape)
{
    switch (shape) {
      case SHAPE_NEXT0:
        jz_ptr(cb, target1);
        break;

      case SHAPE_NEXT1:
        jnz_ptr(cb, target0);
        break;

      case SHAPE_DEFAULT:
        jnz_ptr(cb, target0);
        jmp_ptr(cb, target1);
        break;
    }
}

static codegen_status_t
gen_branchif(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    int32_t jump_offset = (int32_t)jit_get_arg(jit, 0);

    // Check for interrupts, but only on backward branches that may create loops
    if (jump_offset < 0) {
        uint8_t *side_exit = yjit_side_exit(jit, ctx);
        yjit_check_ints(cb, side_exit);
    }

    // Test if any bit (outside of the Qnil bit) is on
    // RUBY_Qfalse  /* ...0000 0000 */
    // RUBY_Qnil    /* ...0000 1000 */
    x86opnd_t val_opnd = ctx_stack_pop(ctx, 1);
    test(cb, val_opnd, imm_opnd(~Qnil));

    // Get the branch target instruction offsets
    uint32_t next_idx = jit_next_insn_idx(jit);
    uint32_t jump_idx = next_idx + jump_offset;
    blockid_t next_block = { jit->iseq, next_idx };
    blockid_t jump_block = { jit->iseq, jump_idx };

    // Generate the branch instructions
    gen_branch(
        jit,
        ctx,
        jump_block,
        ctx,
        next_block,
        ctx,
        gen_branchif_branch
    );

    return YJIT_END_BLOCK;
}

static void
gen_branchunless_branch(codeblock_t *cb, uint8_t *target0, uint8_t *target1, uint8_t shape)
{
    switch (shape) {
      case SHAPE_NEXT0:
        jnz_ptr(cb, target1);
        break;

      case SHAPE_NEXT1:
        jz_ptr(cb, target0);
        break;

      case SHAPE_DEFAULT:
        jz_ptr(cb, target0);
        jmp_ptr(cb, target1);
        break;
    }
}

static codegen_status_t
gen_branchunless(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    int32_t jump_offset = (int32_t)jit_get_arg(jit, 0);

    // Check for interrupts, but only on backward branches that may create loops
    if (jump_offset < 0) {
        uint8_t *side_exit = yjit_side_exit(jit, ctx);
        yjit_check_ints(cb, side_exit);
    }

    // Test if any bit (outside of the Qnil bit) is on
    // RUBY_Qfalse  /* ...0000 0000 */
    // RUBY_Qnil    /* ...0000 1000 */
    x86opnd_t val_opnd = ctx_stack_pop(ctx, 1);
    test(cb, val_opnd, imm_opnd(~Qnil));

    // Get the branch target instruction offsets
    uint32_t next_idx = jit_next_insn_idx(jit);
    uint32_t jump_idx = next_idx + jump_offset;
    blockid_t next_block = { jit->iseq, next_idx };
    blockid_t jump_block = { jit->iseq, jump_idx };

    // Generate the branch instructions
    gen_branch(
        jit,
        ctx,
        jump_block,
        ctx,
        next_block,
        ctx,
        gen_branchunless_branch
    );

    return YJIT_END_BLOCK;
}

static void
gen_branchnil_branch(codeblock_t *cb, uint8_t *target0, uint8_t *target1, uint8_t shape)
{
    switch (shape) {
      case SHAPE_NEXT0:
        jne_ptr(cb, target1);
        break;

      case SHAPE_NEXT1:
        je_ptr(cb, target0);
        break;

      case SHAPE_DEFAULT:
        je_ptr(cb, target0);
        jmp_ptr(cb, target1);
        break;
    }
}

static codegen_status_t
gen_branchnil(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    int32_t jump_offset = (int32_t)jit_get_arg(jit, 0);

    // Check for interrupts, but only on backward branches that may create loops
    if (jump_offset < 0) {
        uint8_t *side_exit = yjit_side_exit(jit, ctx);
        yjit_check_ints(cb, side_exit);
    }

    // Test if the value is Qnil
    // RUBY_Qnil    /* ...0000 1000 */
    x86opnd_t val_opnd = ctx_stack_pop(ctx, 1);
    cmp(cb, val_opnd, imm_opnd(Qnil));

    // Get the branch target instruction offsets
    uint32_t next_idx = jit_next_insn_idx(jit);
    uint32_t jump_idx = next_idx + jump_offset;
    blockid_t next_block = { jit->iseq, next_idx };
    blockid_t jump_block = { jit->iseq, jump_idx };

    // Generate the branch instructions
    gen_branch(
        jit,
        ctx,
        jump_block,
        ctx,
        next_block,
        ctx,
        gen_branchnil_branch
    );

    return YJIT_END_BLOCK;
}

static codegen_status_t
gen_jump(jitstate_t *jit, ctx_t *ctx, codeblock_t *cb)
{
    int32_t jump_offset = (int32_t)jit_get_arg(jit, 0);

    // Check for interrupts, but only on backward branches that may create loops
    if (jump_offset < 0) {
        uint8_t *side_exit = yjit_side_exit(jit, ctx);
        yjit_check_ints(cb, side_exit);
    }

    // Get the branch target instruction offsets
    uint32_t jump_idx = jit_next_insn_idx(jit) + jump_offset;
    blockid_t jump_block = { jit->iseq, jump_idx };

    // Generate the jump instruction
    gen_direct_jump(
        jit,
        ctx,
        jump_block
    );

    return YJIT_END_BLOCK;
}

/*
Guard that self or a stack operand has the same class as `known_klass`, using
`sample_instance` to speculate about the shape of the runtime value.
FIXNUM and on-heap integers are treated as if they have distinct classes, and
the guard generated for one will fail for the other.
Recompile as contingency if possible, or take side exit a last resort.
*/
static bool
jit_guard_known_klass(jitstate_t *jit, ctx_t *ctx, VALUE known_klass, insn_opnd_t insn_opnd, VALUE sample_instance, const int max_chain_depth, uint8_t *side_exit)
{
    val_type_t val_type = ctx_get_opnd_type(ctx, insn_opnd);

    if (known_klass == rb_cNilClass) {
        RUBY_ASSERT(!val_type.is_heap);
        if (val_type.type != ETYPE_NIL) {
            RUBY_ASSERT(val_type.type == ETYPE_UNKNOWN);

            ADD_COMMENT(cb, "guard object is nil");
            cmp(cb, REG0, imm_opnd(Qnil));
            jit_chain_guard(JCC_JNE, jit, ctx, max_chain_depth, side_exit);

            ctx_upgrade_opnd_type(ctx, insn_opnd, TYPE_NIL);
        }
    }
    else if (known_klass == rb_cTrueClass) {
        RUBY_ASSERT(!val_type.is_heap);
        if (val_type.type != ETYPE_TRUE) {
            RUBY_ASSERT(val_type.type == ETYPE_UNKNOWN);

            ADD_COMMENT(cb, "guard object is true");
            cmp(cb, REG0, imm_opnd(Qtrue));
            jit_chain_guard(JCC_JNE, jit, ctx, max_chain_depth, side_exit);

            ctx_upgrade_opnd_type(ctx, insn_opnd, TYPE_TRUE);
        }
    }
    else if (known_klass == rb_cFalseClass) {
        RUBY_ASSERT(!val_type.is_heap);
        if (val_type.type != ETYPE_FALSE) {
            RUBY_ASSERT(val_type.type == ETYPE_UNKNOWN);

            ADD_COMMENT(cb, "guard object is false");
            STATIC_ASSERT(qfalse_is_zero, Qfalse == 0);
            test(cb, REG0, REG0);
            jit_chain_guard(JCC_JNZ, jit, ctx, max_chain_depth, side_exit);

            ctx_upgrade_opnd_type(ctx, insn_opnd, TYPE_FALSE);
        }
    }
    else if (known_klass == rb_cInteger && FIXNUM_P(sample_instance)) {
        RUBY_ASSERT(!val_type.is_heap);
        // We will guard fixnum and bignum as though they were separate classes
        // BIGNUM can be handled by the general else case below
        if (val_type.type != ETYPE_FIXNUM || !val_type.is_imm) {
            RUBY_ASSERT(val_type.type == ETYPE_UNKNOWN);

            ADD_COMMENT(cb, "guard object is fixnum");
            test(cb, REG0, imm_opnd(RUBY_FIXNUM_FLAG));
            jit_chain_guard(JCC_JZ, jit, ctx, max_chain_depth, side_exit);
            ctx_upgrade_opnd_type(ctx, insn_opnd, TYPE_FIXNUM);
        }
    }
    else if (known_klass == rb_cSymbol && STATIC_SYM_P(sample_instance)) {
        RUBY_ASSERT(!val_type.is_heap);
        // We will guard STATIC vs DYNAMIC as though they were separate classes
        // DYNAMIC symbols can be handled by the general else case below
        if (val_type.type != ETYPE_SYMBOL || !val_type.is_imm) {
            RUBY_ASSERT(val_type.type == ETYPE_UNKNOWN);

            ADD_COMMENT(cb, "guard object is static symbol");
            STATIC_ASSERT(special_shift_is_8, RUBY_SPECIAL_SHIFT == 8);
            cmp(cb, REG0_8, imm_opnd(RUBY_SYMBOL_FLAG));
            jit_chain_guard(JCC_JNE, jit, ctx, max_chain_depth, side_exit);
            ctx_upgrade_opnd_type(ctx, insn_opnd, TYPE_STATIC_SYMBOL);
        }
    }
    else if (known_klass == rb_cFloat && FLONUM_P(sample_instance)) {
        RUBY_ASSERT(!val_type.is_heap);
        if (val_type.type != ETYPE_FLONUM || !val_type.is_imm) {
            RUBY_ASSERT(val_type.type == ETYPE_UNKNOWN);

            // We will guard flonum vs heap float as though they were separate classes
            ADD_COMMENT(cb, "guard object is flonum");
            mov(cb, REG1, REG0);
            and(cb, REG1, imm_opnd(RUBY_FLONUM_MASK));
            cmp(cb, REG1, imm_opnd(RUBY_FLONUM_FLAG));
            jit_chain_guard(JCC_JNE, jit, ctx, max_chain_depth, side_exit);
            ctx_upgrade_opnd_type(ctx, insn_opnd, TYPE_FLONUM);
        }
    }
    else if (FL_TEST(known_klass, FL_SINGLETON) && sample_instance == rb_attr_get(known_klass, id__attached__)) {
        // Singleton classes are attached to one specific object, so we can
        // avoid one memory access (and potentially the is_heap check) by
        // looking for the expected object directly.
        // Note that in case the sample instance has a singleton class that
        // doesn't attach to the sample instance, it means the sample instance
        // has an empty singleton class that hasn't been materialized yet. In
        // this case, comparing against the sample instance doesn't guarantee
        // that its singleton class is empty, so we can't avoid the memory
        // access. As an example, `Object.new.singleton_class` is an object in
        // this situation.
        ADD_COMMENT(cb, "guard known object with singleton class");
        // TODO: jit_mov_gc_ptr keeps a strong reference, which leaks the object.
        jit_mov_gc_ptr(jit, cb, REG1, sample_instance);
        cmp(cb, REG0, REG1);
        jit_chain_guard(JCC_JNE, jit, ctx, max_chain_depth, side_exit);
    }
    else {
        RUBY_ASSERT(!val_type.is_imm);

        // Check that the receiver is a heap object
        // Note: if we get here, the class doesn't have immediate instances.
        if (!val_type.is_heap) {
            ADD_COMMENT(cb, "guard not immediate");
            RUBY_ASSERT(Qfalse < Qnil);
            test(cb, REG0, imm_opnd(RUBY_IMMEDIATE_MASK));
            jit_chain_guard(JCC_JNZ, jit, ctx, max_chain_depth, side_exit);
            cmp(cb, REG0, imm_opnd(Qnil));
            jit_chain_guard(JCC_JBE, jit, ctx, max_chain_depth, side_exit);

            ctx_upgrade_opnd_type(ctx, insn_opnd, TYPE_HEAP);
        }

        x86opnd_t klass_opnd = mem_opnd(64, REG0, offsetof(struct RBasic, klass));

        // Bail if receiver class is different from known_klass
        // TODO: jit_mov_gc_ptr keeps a strong reference, which leaks the class.
        ADD_COMMENT(cb, "guard known class");
        jit_mov_gc_ptr(jit, cb, REG1, known_klass);
        cmp(cb, klass_opnd, REG1);
        jit_chain_guard(JCC_JNE, jit, ctx, max_chain_depth, side_exit);
    }

    return true;
}

// Generate ancestry guard for protected callee.
// Calls to protected callees only go through when self.is_a?(klass_that_defines_the_callee).
static void
jit_protected_callee_ancestry_guard(jitstate_t *jit, codeblock_t *cb, const rb_callable_method_entry_t *cme, uint8_t *side_exit)
{
    // See vm_call_method().
    mov(cb, C_ARG_REGS[0], member_opnd(REG_CFP, rb_control_frame_t, self));
    jit_mov_gc_ptr(jit, cb, C_ARG_REGS[1], cme->defined_class);
    // Note: PC isn't written to current control frame as rb_is_kind_of() shouldn't raise.
    // VALUE rb_obj_is_kind_of(VALUE obj, VALUE klass);
    call_ptr(cb, REG0, (void *)&rb_obj_is_kind_of);
    test(cb, RAX, RAX);
    jz_ptr(cb, COUNTED_EXIT(jit, side_exit, send_se_protected_check_failed));
}

// Return true when the codegen function generates code.
// known_recv_klass is non-NULL when the caller has used jit_guard_known_klass().
// See yjit_reg_method().
typedef bool (*method_codegen_t)(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *known_recv_klass);

// Register a specialized codegen function for a particular method. Note that
// the if the function returns true, the code it generates runs without a
// control frame and without interrupt checks. To avoid creating observable
// behavior changes, the codegen function should only target simple code paths
// that do not allocate and do not make method calls.
static void
yjit_reg_method(VALUE klass, const char *mid_str, method_codegen_t gen_fn)
{
    ID mid = rb_intern(mid_str);
    const rb_method_entry_t *me = rb_method_entry_at(klass, mid);

    if (!me) {
        rb_bug("undefined optimized method: %s", rb_id2name(mid));
    }

    // For now, only cfuncs are supported
    RUBY_ASSERT(me && me->def);
    RUBY_ASSERT(me->def->type == VM_METHOD_TYPE_CFUNC);

    st_insert(yjit_method_codegen_table, (st_data_t)me->def->method_serial, (st_data_t)gen_fn);
}

// Codegen for rb_obj_not().
// Note, caller is responsible for generating all the right guards, including
// arity guards.
static bool
jit_rb_obj_not(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *known_recv_klass)
{
    const val_type_t recv_opnd = ctx_get_opnd_type(ctx, OPND_STACK(0));

    if (recv_opnd.type == ETYPE_NIL || recv_opnd.type == ETYPE_FALSE) {
        ADD_COMMENT(cb, "rb_obj_not(nil_or_false)");
        ctx_stack_pop(ctx, 1);
        x86opnd_t out_opnd = ctx_stack_push(ctx, TYPE_TRUE);
        mov(cb, out_opnd, imm_opnd(Qtrue));
    }
    else if (recv_opnd.is_heap || recv_opnd.type != ETYPE_UNKNOWN) {
        // Note: recv_opnd.type != ETYPE_NIL && recv_opnd.type != ETYPE_FALSE.
        ADD_COMMENT(cb, "rb_obj_not(truthy)");
        ctx_stack_pop(ctx, 1);
        x86opnd_t out_opnd = ctx_stack_push(ctx, TYPE_FALSE);
        mov(cb, out_opnd, imm_opnd(Qfalse));
    }
    else {
        // jit_guard_known_klass() already ran on the receiver which should
        // have deduced deduced the type of the receiver. This case should be
        // rare if not unreachable.
        return false;
    }
    return true;
}

// Codegen for rb_true()
static bool
jit_rb_true(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *known_recv_klass)
{
    ADD_COMMENT(cb, "nil? == true");
    ctx_stack_pop(ctx, 1);
    x86opnd_t stack_ret = ctx_stack_push(ctx, TYPE_TRUE);
    mov(cb, stack_ret, imm_opnd(Qtrue));
    return true;
}

// Codegen for rb_false()
static bool
jit_rb_false(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *known_recv_klass)
{
    ADD_COMMENT(cb, "nil? == false");
    ctx_stack_pop(ctx, 1);
    x86opnd_t stack_ret = ctx_stack_push(ctx, TYPE_FALSE);
    mov(cb, stack_ret, imm_opnd(Qfalse));
    return true;
}

// Codegen for rb_obj_equal()
// object identity comparison
static bool
jit_rb_obj_equal(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *known_recv_klass)
{
    ADD_COMMENT(cb, "equal?");
    x86opnd_t obj1 = ctx_stack_pop(ctx, 1);
    x86opnd_t obj2 = ctx_stack_pop(ctx, 1);

    mov(cb, REG0, obj1);
    cmp(cb, REG0, obj2);
    mov(cb, REG0, imm_opnd(Qtrue));
    mov(cb, REG1, imm_opnd(Qfalse));
    cmovne(cb, REG0, REG1);

    x86opnd_t stack_ret = ctx_stack_push(ctx, TYPE_IMM);
    mov(cb, stack_ret, REG0);
    return true;
}

static VALUE
yjit_str_bytesize(VALUE str)
{
    return LONG2NUM(RSTRING_LEN(str));
}

static bool
jit_rb_str_bytesize(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *known_recv_klass)
{
    ADD_COMMENT(cb, "String#bytesize");

    x86opnd_t recv = ctx_stack_pop(ctx, 1);
    mov(cb, C_ARG_REGS[0], recv);
    call_ptr(cb, REG0, (void *)&yjit_str_bytesize);

    x86opnd_t out_opnd = ctx_stack_push(ctx, TYPE_FIXNUM);
    mov(cb, out_opnd, RAX);

    return true;
}

// Codegen for rb_str_to_s()
// When String#to_s is called on a String instance, the method returns self and
// most of the overhead comes from setting up the method call. We observed that
// this situation happens a lot in some workloads.
static bool
jit_rb_str_to_s(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *recv_known_klass)
{
    if (recv_known_klass && *recv_known_klass == rb_cString) {
        ADD_COMMENT(cb, "to_s on plain string");
        // The method returns the receiver, which is already on the stack.
        // No stack movement.
        return true;
    }
    return false;
}

static bool
jit_thread_s_current(jitstate_t *jit, ctx_t *ctx, const struct rb_callinfo *ci, const rb_callable_method_entry_t *cme, rb_iseq_t *block, const int32_t argc, VALUE *recv_known_klass)
{
    ADD_COMMENT(cb, "Thread.current");
    ctx_stack_pop(ctx, 1);

    // ec->thread_ptr
    mov(cb, REG0, member_opnd(REG_EC, rb_execution_context_t, thread_ptr));

    // thread->self
    mov(cb, REG0, member_opnd(REG0, rb_thread_t, self));

    x86opnd_t stack_ret = ctx_stack_push(ctx, TYPE_HEAP);
    mov(cb, stack_ret, REG0);
    return true;
}

// Check if we know how to codegen for a particular cfunc method
static method_codegen_t
lookup_cfunc_codegen(const rb_method_definition_t *def)
{
    method_codegen_t gen_fn;
    if (st_lookup(yjit_method_codegen_table, def->method_serial, (st_data_t *)&gen_fn)) {
        return gen_fn;
    }
    return NULL;
}

// Is anyone listening for :c_call and :c_return event currently?
static bool
c_method_tracing_currently_enabled(const jitstate_t *jit)
{
    rb_event_flag_t tracing_events;
    if (rb_multi_ractor_p()) {
        tracing_events = ruby_vm_event_enabled_global_flags;
    }
    else {
        // At the time of writing, events are never removed from
        // ruby_vm_event_enabled_global_flags so always checking using it would
        // mean we don't compile even after tracing is disabled.
        tracing_events = rb_ec_ractor_hooks(jit->ec)->events;
    }

    return tracing_events & (RUBY_EVENT_C_CALL | RUBY_EVENT_C_RETURN);
}

// Called at runtime to build hashes of passed kwargs
static VALUE
yjit_runtime_build_kwhash(const struct rb_callinfo *ci, const VALUE *sp) {
    // similar to args_kw_argv_to_hash
    const VALUE *const passed_keywords = vm_ci_kwarg(ci)->keywords;
    const int kw_len = vm_ci_kwarg(ci)->keyword_len;
    const VALUE h = rb_hash_new_with_size(kw_len);

    for (int i = 0; i < kw_len; i++) {
        rb_hash_aset(h, passed_keywords[i], (sp - kw_len)[i]);
    }
    return h;
}

static codegen_status_t
