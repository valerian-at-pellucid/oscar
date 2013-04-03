package oscar.examples.cp.BinPacking.Tester2

import scala.util.Random
import oscar.examples.cp.BinPacking.DataStructures.BinPackingInstance
import scala.collection.immutable.Stream.consWrapper

class BinPackingValidInstanceGenerator {
	var numberOfBins = 10
	
	var itemsByBinsLimits 		= (2,100)
	var itemsByBinsMean 		= 30
	var itemsByBinsDeviation 	= 10
	
	var itemsSizeLimits 		= (1,100)
	var itemsSizeMean			= 10
	var itemsSizeDeviation		= 4
	
	var binSizeDomainExpension 	= 0.10
	
	var binByItemMean			= 2
	var binByItemDeviation		= 1
	
	def key = List("numberOfBins","itemsByBinsLimits","itemsByBinsMean","itemsByBinsDeviation","itemsSizeLimits","itemsSizeMean","itemsSizeDeviation","binSizeDomainExpension","binByItemMean","binByItemDeviation")
	def values = List(numberOfBins,itemsByBinsLimits,itemsByBinsMean,itemsByBinsDeviation,itemsSizeLimits,itemsSizeMean,itemsSizeDeviation,binSizeDomainExpension,binByItemMean,binByItemDeviation)
	
	def generateSolution : List[List[Int]] = {
	  (0 until numberOfBins).map{ 
	    i =>
		  val r = new util.Random
	      val numberOfItemsInBin = (r.nextGaussian * itemsByBinsDeviation + itemsByBinsMean).toInt
	      val binConstitutions = Stream.continually((r.nextGaussian * itemsSizeDeviation + itemsSizeMean).toInt).filterNot(r => r < itemsByBinsLimits._1 || r > itemsByBinsLimits._2).take(numberOfItemsInBin).toArray   
	      binConstitutions.toList
	  }.toList
	}
	
	def generateInstance() : BinPackingInstance = 
	{
	  var solution = generateSolution 
	  while(solution.flatten[Int].length == 0)
	    solution = generateSolution
	  
	  val r = new Random
	  val bpi = new BinPackingInstance()
	  bpi.binCapacities = solution.map{
	    items => 
	      val capacity = items.sum
	      (capacity * (1-binSizeDomainExpension)).toInt to (capacity * (1+binSizeDomainExpension)).toInt
	  }.toArray
	  
	  bpi.itemsSizes = solution.flatten[Int].toArray
	  
	  bpi.binForItems = solution.zipWithIndex.map
	  {
	    case (items, binIndex) =>
	      items.map
	      {
	        item =>
	    	  var numberOfItem = Math.min(solution.length, Math.max((r.nextGaussian * binByItemDeviation + binByItemMean).toInt,1))
	    	  (binIndex :: Random.shuffle((0 until numberOfBins).toList).filterNot(_==binIndex).take(numberOfItem-1)).toArray
	    	 
	    	  
	      }
	  }.flatten.toArray
	  
	  bpi
	}
	
	
	def generate () : Stream[BinPackingInstance] = 
	{
	  generateInstance #:: generate()  
	}
}