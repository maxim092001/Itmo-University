#if !defined(FIELD) || !defined(VAR_FIELD)
#  error You need to define FIELD and VAR_FIELD macro
#else

VAR_FIELD(cl_ord_id, 14)
FIELD(side, char, char)
FIELD(volume, binary4, unsigned)
FIELD(symbol, binary4, unsigned)
FIELD(price, price, double)
VAR_FIELD(firm, 4)
VAR_FIELD(user, 6)

#undef FIELD
#undef VAR_FIELD
#endif