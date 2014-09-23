/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/**
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.parser.intermediatemodel;

import java.util.ArrayList;
import java.util.List;

import oscar.flatzinc.model.Annotation;
import oscar.flatzinc.model.Domain;

public class Element {
	static int nextid = 0;
	public String name;
	int id;
	public Object value;
	public Domain domain;
	public Type typ;
	public List<Annotation> annotations;
	public Element(){
		id = nextid;
		nextid++;
		annotations = new ArrayList<Annotation>();
	}
	@Override
	public String toString() {
		return "Element [name=" + name + ", value=" + value + ", domain=" + domain + ", type=" + typ
				+ ", annotations=" + annotations + "]";
	}
	
}
