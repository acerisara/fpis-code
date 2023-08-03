package fpis.code.chapter12

import fpis.code.chapter12.Traverse.Tree
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TraverseTest extends AnyFunSuite {

  private val listTraverse = Traverse.listTraverse
  private val treeTraverse = Traverse.treeTraverse

  implicit val optionApplicative: Applicative[Option] =
    Applicative.optionApplicative

  test("List traverse") {
    listTraverse.traverse(List.empty[Int])(a => Option(a)) should be(
      Some(List.empty)
    )

    listTraverse.traverse(List(1, 2, 3))(a => Option(a)) should be(
      Some(List(1, 2, 3))
    )

    listTraverse.sequence(List.empty[Option[Int]]) should be(
      Some(List.empty)
    )

    listTraverse.sequence(List(Option(1), Option(2), Option(3))) should be(
      Some(List(1, 2, 3))
    )
  }

  test("Tree traverse") {
    val tree = Tree(1, List(Tree(2, List(Tree(3, List.empty)))))

    treeTraverse.traverse(tree)(a => Option(a)) should be(
      Some(Tree(1, List(Tree(2, List(Tree(3, List.empty))))))
    )

    val optionTree = treeTraverse.map(tree)(Option(_))

    treeTraverse.sequence(optionTree) should be(
      Some(Tree(1, List(Tree(2, List(Tree(3, List.empty))))))
    )
  }

}
