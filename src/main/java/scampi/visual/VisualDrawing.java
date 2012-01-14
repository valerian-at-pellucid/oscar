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


import java.awt.Color;


import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.LinkedList;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import javax.swing.TransferHandler;
import javax.swing.event.CellEditorListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;


import sun.misc.Cleaner;



public class VisualDrawing extends JPanel  {

	JPanel drawingPanel;
	

	LinkedList<ColoredShape> shapes;

	public VisualDrawing(boolean saveButton) {
		super(new BorderLayout());
		
		shapes = new LinkedList<ColoredShape>();
		
		drawingPanel = new JPanel() {
			
			public void paintComponent(Graphics g) {
			
				super.paintComponent(g);
				for (ColoredShape s: shapes) {
					s.draw((Graphics2D)g);
				}
			}
		};
		
		drawingPanel.setBackground(Color.white);

		

		add(drawingPanel, BorderLayout.CENTER);


		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		buttonPanel.setBackground(Color.white);
		ImageIcon icon = new ImageIcon(VisualDrawing.class.getResource("icon/save.png"));

		Image img = icon.getImage();  
		Image newimg = img.getScaledInstance(30, 30,  java.awt.Image.SCALE_SMOOTH);  
		icon = new ImageIcon(newimg);  


		final JButton export = new JButton(icon);
		export.setSize(30, 30);
		export.addActionListener(new ActionListener() {	
			@Override
			public void actionPerformed(ActionEvent arg0) {

			}
		});

		buttonPanel.add(export);

		if (saveButton) {
			add(buttonPanel, BorderLayout.NORTH);			
		}


	}


	public void addShape(ColoredShape s) {
		shapes.add(s);
	}






}
