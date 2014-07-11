package fr.putnami.pwt.core.security.client.widget;

import java.util.Collections;
import java.util.Iterator;

import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.HasOneWidget;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

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
			eval();
		}
	};
	private final SignOutEvent.Handler signoutHandler = new SignOutEvent.Handler() {

		@Override
		public void onSignOutEvent(SignOutEvent event) {
			eval();
		}
	};

	private String path;
	private Widget widget;

	private boolean negate = false;
	private String hasRole;

	public Secure() {
		SessionController.get().addSignInHandler(signinHandler);
		SessionController.get().addSignOutHandler(signoutHandler);
	}

	public Secure(Secure source) {
		setWidget(WidgetUtils.cloneWidget(source.widget));
	}
	@Override
	public String getPath() {
		return path == null ? Path.ROOT_PATH : path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public Iterable<Editor> getEditors() {
		if (getWidget() instanceof Editor) {
			return Lists.newArrayList((Editor) getWidget());
		}
		else {
			return Collections.EMPTY_LIST;
		}
	}

	@Override
	public IsWidget cloneWidget() {
		return new Secure(this);
	}

	@Override
	public Widget getWidget() {
		return widget;
	}

	@Override
	public void setWidget(IsWidget w) {
		this.setWidget(w == null ? null : w.asWidget());
	}

	@Override
	public void setWidget(Widget w) {
		if (w == widget) {
			return;
		}
		widget = w;
		if (widget == null) {
			return;
		}
	}

	@Override
	public void add(Widget w) {
		if (getWidget() != null) {
			throw new IllegalStateException("can only contain one child widget");
		}
		this.setWidget(w);
	}

	@Override
	public void clear() {
		widget = null;
	}

	@Override
	public Iterator<Widget> iterator() {
		return Lists.newArrayList(widget).iterator();
	}

	@Override
	public boolean remove(Widget w) {
		if (widget != w) {
			return false;
		}
		widget = null;
		return true;
	}

	@Override
	public Widget asWidget() {
		eval();
		return getWidget();
	}

	public boolean isNegate() {
		return negate;
	}

	public void setNegate(boolean negate) {
		this.negate = negate;
	}

	public String getHasRole() {
		return hasRole;
	}

	public void setHasRole(String hasRole) {
		this.hasRole = hasRole;
	}

	public void eval() {
		SessionController controller = SessionController.get();
		boolean display = true;
		if (hasRole != null) {
			display = !negate && controller.hasRole(hasRole);
		}
		getWidget().setVisible(display);
	}
}
