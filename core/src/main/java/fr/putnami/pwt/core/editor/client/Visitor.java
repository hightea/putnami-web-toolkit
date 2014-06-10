/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.editor.client;

import java.util.Comparator;

import com.google.common.base.Predicate;
import com.google.common.primitives.Ints;

public interface Visitor {

	int PRECEDENCE_NORMAL = 0;

	class VisitorComparator implements Comparator<Visitor> {

		@Override
		public int compare(Visitor o1, Visitor o2) {
			if (o1 == null || o2 == null) {
				return 0;
			}
			return Ints.compare(o2.getPrecedence(), o1.getPrecedence());
		}

	}

	enum VisitorTrigger implements Predicate<Visitor> {
		INITALIZE, BEFORE_EDIT, AFTER_EDIT, FLUSH, MANUAL;

		@Override
		public boolean apply(Visitor input) {
			return (input != null) && equals(input.trigerOn());
		}

	}

	int getPrecedence();

	VisitorTrigger trigerOn();

	<A, B extends Editor> boolean beforeVisit();

	<A, B extends Editor> boolean visit(Context<B> context);

	<A, B extends Editor> boolean afterVisit();
}
