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

import com.google.common.collect.Lists;
import com.google.web.bindery.event.shared.HandlerRegistration;

import java.util.Collection;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent.Handler;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;

public class DirtyEventRegistrerVisitor extends AbstractVisitor {

	private final Handler handler;
	private final Collection<HandlerRegistration> handlerRegistration = Lists.newArrayList();

	public DirtyEventRegistrerVisitor(Handler handler) {
		this.handler = handler;
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		B editor = context.getEditor();
		if (editor instanceof DirtyEvent.HasDirtyHandlers) {
			this.handlerRegistration.add(((DirtyEvent.HasDirtyHandlers) editor)
					.addDirtyHandler(this.handler));
		}
		return true;
	}

	public Collection<HandlerRegistration> getHandlerRegistrations() {
		return this.handlerRegistration;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.INITALIZE;
	}

}
