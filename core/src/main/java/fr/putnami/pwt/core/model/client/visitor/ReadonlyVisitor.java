/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.model.client.visitor;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.model.client.base.HasReadonly;
import fr.putnami.pwt.core.model.client.visitor.ReadonlyAspectVisitor.ReadonlyAspect;

public class ReadonlyVisitor extends AbstractVisitor {

	private final boolean readonly;
	private Editor source;

	public ReadonlyVisitor(Editor source, Boolean readonly, boolean defaultReadonly) {
		this.readonly = readonly == null ? defaultReadonly : readonly;
		this.source = source;
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		ReadonlyAspect readonlyAspect = context.getAspect(ReadonlyAspect.class);
		Editor editor = context.getEditor();
		if (this.source != editor) {
			if (readonlyAspect != null && editor instanceof HasReadonly) {
				HasReadonly hasReadonly = (HasReadonly) editor;
				boolean isReadonly = true;
				if (!Boolean.TRUE.equals(readonlyAspect.getDefaultReadonly())) {
					isReadonly = this.readonly;
				}
				hasReadonly.setReadonly(isReadonly);
			}
			if (editor instanceof HasDrawable) {
				((HasDrawable) editor).redraw();
			}
		}
		return true;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.MANUAL;
	}

}
