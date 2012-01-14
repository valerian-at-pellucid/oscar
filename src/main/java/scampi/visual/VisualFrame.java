/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.visual;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

public class VisualFrame extends JFrame {


	JDesktopPane desktop;
	
	public VisualFrame(String title) {
		super(title);

		addWindowListener(new WindowAdapter() {
			 public void windowClosing(WindowEvent event) {
				    System.exit(0);
			  }
		});
		
		Container content = getContentPane();
		content.setBackground(Color.white);
		desktop = new JDesktopPane();
		desktop.setBackground(Color.white);
		content.add(new JScrollPane(desktop));
		//content.add(desktop);
		
		
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		setSize(screenSize);
		setVisible(true);

	}
	
	
	
	protected JMenuBar createMenuBar() {
		JMenuBar menuBar = new JMenuBar();
		JMenu menu = new JMenu("Frame");
		menu.setMnemonic(KeyEvent.VK_N);
		JMenuItem menuItem = new JMenuItem("New IFrame");
		menuItem.setMnemonic(KeyEvent.VK_N);
		menuItem.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				createFrame("sub frame");
			}
		});
		menu.add(menuItem);
		menuBar.add(menu);
		return menuBar;
	}
	
	public JInternalFrame createFrame(String title) {

		JInternalFrame frame = new JInternalFrame(title, true, false, true, true);
		
		frame.setLocation(10,10);
		frame.setSize(200, 150);
		frame.setBackground(Color.white);
		frame.setVisible(true);
		desktop.add(frame);
		frame.moveToFront();		
		return frame;
	}	


	public static void main(String[] args) {
		VisualFrame frame = new VisualFrame("My Frame");
		JInternalFrame subframe = frame.createFrame("My Sub-frame");
	}

}
