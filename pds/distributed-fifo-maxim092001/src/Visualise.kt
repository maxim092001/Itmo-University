package mutex

import java.awt.*
import java.awt.event.*
import javax.swing.*
import javax.swing.event.*
import javax.swing.plaf.*
import kotlin.math.*

fun main() = SwingUtilities.invokeAndWait {
    val model = Model(2)
    val control = ControlPane(model)
    JFrame("Visualisation").apply {
        add(control, BorderLayout.NORTH)
        contentPane.add(VScrollPane(model), BorderLayout.CENTER)
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        pack()
        isLocationByPlatform = true
        isVisible = true
    }
    control.load()
    control.installActionListeners()
}

val IMPLS = listOf(
    "",
    "ProcessLamportMutex",
    "ProcessRickartAgrawalaMutex",
    "ProcessSyncCoordinatedMutex",
    "ProcessTokenMutex"
)

val BG_COLOR: Color = Color.WHITE
val FG_COLOR: Color = Color.BLACK
val HOVER_COLOR: Color = Color.GRAY
val LOCK_COLOR: Color = Color.RED

class VScrollPane(model: Model) :
    JScrollPane(VComponent(model), VERTICAL_SCROLLBAR_ALWAYS, HORIZONTAL_SCROLLBAR_NEVER),
    ChangeListener
{
    private val vm = verticalScrollBar.model
    private var wasAtBottom = true
    private var oldMax = vm.maximum

    init {
        vm.addChangeListener(this)
    }

    override fun stateChanged(e: ChangeEvent?) {
        if (wasAtBottom && vm.maximum != oldMax) {
            SwingUtilities.invokeLater { moveToBottom() }
        }
        wasAtBottom = isBottom()
    }

    private fun isBottom(): Boolean = vm.value + vm.extent >= vm.maximum

    private fun moveToBottom() {
        vm.value = vm.maximum - vm.extent
        wasAtBottom = true
        oldMax = vm.maximum
    }
}

class VComponent(model: Model) : JComponent() {
    init {
        val ui = VComponentUI(model, this)
        addMouseMotionListener(ui)
        addMouseListener(ui)
        model.updateListener = {
            revalidate()
            repaint()
        }
        this.ui = ui
        background = BG_COLOR
        foreground = FG_COLOR
        isOpaque = true
    }
}

const val X_PAD = 250
const val X_SPACE = 100
const val Y_TOP = 50
const val Y_MINIMUM = 800
const val TEXT_PAD = 10

const val R = 10
const val R1 = 7
const val X_LOCK = 5

fun xProcess(processId: Int) = X_PAD + X_SPACE * (processId - 1)
fun yTime(time: Int) = Y_TOP + time

fun processByX(x: Int) = ((x - X_PAD) / X_SPACE.toDouble()).roundToInt() + 1
fun timeByY(y: Int) = y - Y_TOP

class VComponentUI(
    private val model: Model,
    private val c: JComponent
) : ComponentUI(), MouseMotionListener, MouseListener {
    private var hover: Action? = null
    private var dragT0 = 0
    private var dragStart = Point(0, 0)

    override fun paint(g: Graphics, c: JComponent) {
        g as Graphics2D
        g.color = FG_COLOR
        for (i in 1..model.nProcesses) {
            val x = xProcess(i)
            val y = yTime(0)
            g.drawLine(x, y, x, yTime(model.time + T_STEP))
            val s = i.toString()
            val b = g.fontMetrics.getStringBounds(s, g)
            g.drawString(s, x - (b.width / 2).toFloat(), y.toFloat() - TEXT_PAD)
        }
        for (rcvd in model.actions.filterIsInstance<Rcvd>()) {
            val send = rcvd.from
            g.drawLine(
                xProcess(send.processId), yTime(send.time),
                xProcess(rcvd.processId), yTime(rcvd.time)
            )
        }
        for (p in model.ps.values) {
            for (a in p.actions) {
                when (a) {
                    is LockRequest -> paintLockRequest(g, a)
                    is Unlock -> paintLock(g, a)
                    is Send, is Rcvd -> paintSendRcvd(g, a)
                    is Lock -> {}
                }
            }
        }
        hover?.let { hover ->
            val x = xProcess(hover.processId)
            val y = yTime(hover.time)
            g.color = HOVER_COLOR
            g.fillOval(x - R1, y - R1, 2 * R1, 2 * R1)
        }
    }

    private fun paintLockRequest(g: Graphics2D, a: LockRequest) {
        val x = xProcess(a.processId)
        val y = yTime(a.time)
        g.color = FG_COLOR
        g.drawLine(x - R, y, x + R, y)
    }

    private fun paintLock(g: Graphics2D, a: Unlock) {
        g.color = LOCK_COLOR
        val x = xProcess(a.processId)
        val y1 = yTime(a.from.time)
        val y2 = yTime(a.time)
        g.fillRect(x - X_LOCK, y1, 2 * X_LOCK, y2 - y1)
    }

    private fun paintSendRcvd(g: Graphics2D, a: Action) {
        val x = xProcess(a.processId)
        val y = yTime(a.time)
        g.color = if (a is Send) BG_COLOR else FG_COLOR
        g.fillOval(x - R, y - R, 2 * R, 2 * R)
        g.color = FG_COLOR
        g.drawOval(x - R, y - R, 2 * R, 2 * R)
        if (a is Send && a !in model.pending) {
            g.color = FG_COLOR
            g.fillOval(x - R1, y - R1, 2 * R1, 2 * R1)
        }
        g.color = FG_COLOR
        val s = a.toString()
        val b = g.fontMetrics.getStringBounds(s, g)
        val sx = if (a.processId == 1)
            x - b.width.toFloat() - R - TEXT_PAD else
            x + R + TEXT_PAD.toFloat()
        g.drawString(s, sx, y.toFloat())

    }

    override fun getPreferredSize(c: JComponent?): Dimension =
        Dimension(
            2 * X_PAD + X_SPACE * (model.nProcesses - 1),
            maxOf(yTime(model.time + T_STEP), Y_MINIMUM)
        )

    private fun MouseEvent.action(): Action? {
        val i = processByX(x)
        val time = timeByY(y)
        val action = model.ps[i]?.actions?.minByOrNull { abs(time - it.time) } ?: return null
        if (point.distance(Point(xProcess(i), yTime(action.time))) > R) return null
        return action
    }

    private fun hover(hover: Action?) {
        if (this.hover == hover) return
        this.hover = hover
        c.repaint()
    }

    override fun mouseClicked(e: MouseEvent) {
        val send = e.action() as? Send ?: return
        if (send in model.pending) model.perform(send)
    }

    override fun mouseMoved(e: MouseEvent) = hover(e.action()?.takeIf { it.time > 0 })
    override fun mouseExited(e: MouseEvent) = hover(null)

    override fun mousePressed(e: MouseEvent) {
        val hover = hover ?: return
        dragT0 = hover.time
        dragStart = e.point
    }

    override fun mouseDragged(e: MouseEvent) {
        val hover = hover ?: return
        val t1 = dragT0 + e.y - dragStart.y
        if (t1 > dragT0) {
            val next = model.nextAction(hover)
            var time = next?.time ?: Int.MAX_VALUE
            hover.to?.let { a -> time = minOf(time, a.time) }
            if (t1 >= time - R) return
        } else {
            val prev = model.prevAction(hover)
            var time = prev?.time ?: 0
            hover.from?.let { a -> time = maxOf(time, a.time) }
            if (t1 <= time + R) return
        }
        hover.time = t1
        c.revalidate()
        c.repaint()
    }

    override fun mouseReleased(e: MouseEvent) {}
    override fun mouseEntered(e: MouseEvent) {}
}

class ControlPane(private val model: Model) : JPanel(FlowLayout()), ActionListener {
    private val name = JTextField(15)
    private val impl = JComboBox(IMPLS.toTypedArray())
    private val restart = JButton("Restart")
    private val save = JButton("Save")

    init {
        add(JLabel("Name"))
        add(name)
        add(JLabel("Impl"))
        add(impl)
        add(restart)
        add(save)
    }

    override fun actionPerformed(e: ActionEvent) {
        when (e.source) {
            save -> model.save(name.text, SOLUTION_FILE)
            else -> model.restart(name.text, impl.item)
        }
    }

    fun load() {
        if (!SOLUTION_FILE.exists()) return
        model.load(SOLUTION_FILE)
        name.text = model.name
        impl.selectedItem = model.impl
    }

    fun installActionListeners() {
        name.addActionListener(this)
        impl.addActionListener(this)
        restart.addActionListener(this)
        save.addActionListener(this)
    }
}

private val <E> JComboBox<E>.item: E
    get() = getItemAt(selectedIndex)
