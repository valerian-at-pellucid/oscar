/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.search;

/**
 * Dummy Objective doing nothing (used for feasibility problems)
 * @author pschaus@gmail.com
 */
public class DummyObjective extends Objective {

	@Override
	public void tighten() {		
	}

	@Override
	public void relax() {		
	}

	@Override
	public void setNewBound(int val) {		
	}

	@Override
	public int getBound() {
		return 0;
	}

	@Override
	public int getOptimumBound() {
		return 0;
	}

	@Override
	public boolean isOptimum() {
		return false;
	}
	
	@Override
	public boolean isOK() {
		return true;
	}

}
