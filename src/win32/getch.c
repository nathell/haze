#include <curses.h>

/* There is a problem with either PDCurses or GHC's FFI which causes the getch() call
 * to return 224 instead of the proper values when function/arrow/... keys are pressed.
 * This dummy overlay seems to solve the problem.
 */
int dummy_getch(void) 
{
	return getch();
}
