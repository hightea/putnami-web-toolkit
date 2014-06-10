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
package fr.putnami.pwt.core.model.client.visitor;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.aspect.ContextAspect;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.base.HasReadonly;

public class ReadonlyAspectVisitor extends AbstractVisitor {
	public class ReadonlyAspect implements ContextAspect {
		private final Boolean defaultReadonly;

		public ReadonlyAspect(Boolean defaultReadonly) {
			super();
			this.defaultReadonly = defaultReadonly;
		}

		public Boolean getDefaultReadonly() {
			return defaultReadonly;
		}
	}

	public ReadonlyAspectVisitor() {
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		Editor editor = context.getEditor();
		if (editor instanceof HasReadonly) {
			HasReadonly hasReadonly = (HasReadonly) editor;
			context.addAspect(new ReadonlyAspect(hasReadonly.getReadonly()));
		}
		return true;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.INITALIZE;
	}

}
