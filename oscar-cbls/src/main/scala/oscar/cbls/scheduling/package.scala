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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls

import scheduling.algo.IFlatIRelax
import scheduling.model.{Planning, SuperActivity, Activity, CumulativeResource}

/**
 * THIS PACKAGE IS EXPERIMENTAL
 *
 *  This package is a scheduling library.
 * it supports
 - [[oscar.cbls.scheduling.model.CumulativeResource]]
 - [[oscar.cbls.scheduling.model.Activity]] with varying durations and precedence constraints
 - [[oscar.cbls.scheduling.model.SuperActivity]] that align their start and end to other tasks.
 *   This is useful to model that a resource is not released between tasks.
 *
 * In this package, Tasks are grouped into [[oscar.cbls.scheduling.model.Planning]] that keeps references to all tasks and resources.
 * This package features the [[oscar.cbls.scheduling.algo.IFlatIRelax]] search heuristics with various tunings
  * @author renaud.delandtsheer@cetic.be
 * */
package object scheduling{
}

