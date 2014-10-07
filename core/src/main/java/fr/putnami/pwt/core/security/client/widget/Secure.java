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
package fr.putnami.pwt.core.security.client.widget;

import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.HasOneWidget;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import java.util.Collections;
import java.util.Iterator;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorComposite;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.security.client.controller.SessionController;
import fr.putnami.pwt.core.security.client.event.SignInEvent;
import fr.putnami.pwt.core.security.client.event.SignOutEvent;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class Secure implements IsWidget, HasWidgets, HasOneWidget, CloneableWidget, EditorComposite {

	private final SignInEvent.Handler signinHandler = new SignInEvent.Handler() {

		@Override
		public void onSignInEvent(SignInEvent event) {
			Secure.this.eval();
		}
	};
	private final SignOutEvent.Handler signoutHandler = new SignOutEvent.Handler() {

		@Override
		public void onSignOutEvent(SignOutEvent event) {
			Secure.this.eval();
		}
	};

	private String path;
	private Widget widget;

	private boolean negate = false;
	private String hasRole;

	public Secure() {
		SessionController.get().addSignInHandler(this.signinHandler);
		SessionController.get().addSignOutHandler(this.signoutHandler);
	}

	public Secure(Secure source) {
		this.setWidget(WidgetUtils.cloneWidget(source.widget));
	}

	@Override
	public String getPath() {
		return this.path == null ? Path.ROOT_PATH : this.path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public Iterable<Editor> getEditors() {
		if (this.getWidget() instanceof Editor) {
			return Lists.newArrayList((Editor) this.getWidget());
		}
		return Collections.<Editor> emptyList();
	}

	@Override
	public IsWidget cloneWidget() {
		return new Secure(this);
	}

	@Override
	public Widget getWidget() {
		return this.widget;
	}

	@Override
	public void setWidget(IsWidget w) {
		this.setWidget(w == null ? null : w.asWidget());
	}

	@Override
	public void setWidget(Widget w) {
		if (w == this.widget) {
			return;
		}
		this.widget = w;
		if (this.widget == null) {
			return;
		}
	}

	@Override
	public void add(Widget w) {
		if (this.getWidget() != null) {
			throw new IllegalStateException("can only contain one child widget");
		}
		this.setWidget(w);
	}

	@Override
	public void clear() {
		this.widget = null;
	}

	@Override
	public Iterator<Widget> iterator() {
		return Lists.newArrayList(this.widget).iterator();
	}

	@Override
	public boolean remove(Widget w) {
		if (this.widget != w) {
			return false;
		}
		this.widget = null;
		return true;
	}

	@Override
	public Widget asWidget() {
		this.eval();
		return this.getWidget();
	}

	public boolean isNegate() {
		return this.negate;
	}

	public void setNegate(boolean negate) {
		this.negate = negate;
	}

	public String getHasRole() {
		return this.hasRole;
	}

	public void setHasRole(String hasRole) {
		this.hasRole = hasRole;
	}

	public void eval() {
		SessionController controller = SessionController.get();
		boolean display = true;
		if (this.hasRole != null) {
			display = !this.negate && controller.hasRole(this.hasRole);
		}
		this.getWidget().setVisible(display);
	}
}
