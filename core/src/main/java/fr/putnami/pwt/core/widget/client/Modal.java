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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Style.Display;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.HasOneWidget;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.base.HasFooter;
import fr.putnami.pwt.core.widget.client.base.HasHeader;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class Modal extends AbstractPanel implements
		HasOneWidget,
		CloneableWidget,
		HasDrawable {

	private static final CssStyle STYLE_MODAL = new SimpleStyle("modal");
	private static final CssStyle STYLE_DIALOG = new SimpleStyle("modal-dialog");
	private static final CssStyle STYLE_CONTENT = new SimpleStyle("modal-content");
	private static final CssStyle STYLE_HEADER = new SimpleStyle("modal-header");
	private static final CssStyle STYLE_BODY = new SimpleStyle("modal-body");
	private static final CssStyle STYLE_FOOTER = new SimpleStyle("modal-footer");
	private static final CssStyle STYLE_BACKDROP = new SimpleStyle("modal-backdrop");

	private static final CssStyle STYLE_FADE = new SimpleStyle("fade");
	private static final CssStyle STYLE_VISIBLE = new SimpleStyle("in");
	private static final CssStyle STYLE_CLOSE = new SimpleStyle("close");

	private static class ModalBackdrop extends Widget {

		public ModalBackdrop() {
			setElement(Document.get().createDivElement());
			StyleUtils.addStyle(this, STYLE_BACKDROP);
			StyleUtils.addStyle(this, STYLE_FADE);
		}

		public void show() {
			RootPanel.get().add(this);
			Scheduler.get().scheduleDeferred(new ScheduledCommand() {
				@Override
				public void execute() {
					StyleUtils.addStyle(ModalBackdrop.this, STYLE_VISIBLE);
				}
			});
		}

		public void hide() {
			StyleUtils.removeStyle(ModalBackdrop.this, STYLE_VISIBLE);
			Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

				@Override
				public boolean execute() {
					RootPanel.get().remove(ModalBackdrop.this);
					return false;
				}
			}, 100);
		}
	}

	private static final ModalBackdrop MODAL_BACKDROP = new ModalBackdrop();

	private final Container headerContainer = new Container(DivElement.TAG);
	private final Container dialogContainer = new Container(DivElement.TAG);
	private final Container contentContainer = new Container(DivElement.TAG);
	private final Container bodyContainer = new Container(DivElement.TAG);

	private Anchor<?> dismissButton;
	private Header header;
	private Widget widget;
	private Footer footer;

	private boolean visible = false;
	private boolean dismissable;
	private String title;

	public Modal() {
		super(DivElement.TAG);

		StyleUtils.addStyle(this, STYLE_MODAL);
		StyleUtils.addStyle(this, STYLE_FADE);

		append(dialogContainer);
		dialogContainer.append(contentContainer);

		StyleUtils.addStyle(dialogContainer, STYLE_DIALOG);
		StyleUtils.addStyle(contentContainer, STYLE_CONTENT);
		StyleUtils.addStyle(bodyContainer, STYLE_BODY);
	}

	protected Modal(Modal source) {
		super(source);

		this.visible = source.visible;
		this.dismissable = source.dismissable;
		this.title = source.title;

		setHeader(WidgetUtils.cloneWidget(header));
		setFooter(WidgetUtils.cloneWidget(footer));
		setWidget(WidgetUtils.cloneWidget(widget));
		throw new UnsupportedOperationException("not implemented yet");
	}

	@Override
	public IsWidget cloneWidget() {
		return new Modal(this);
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof Header) {
			setHeader((Header) w);
			return;
		}
		if (w instanceof Footer) {
			setFooter((Footer) w);
			return;
		}
		if (w instanceof HasHeader) {
			setHeader(((HasHeader) w).getHeader());
		}
		if (w instanceof HasFooter) {
			setFooter(((HasFooter) w).getFooter());
		}
		setWidget(w);
	}

	public void setHeader(Header header) {
		if (header == null) {
			return;
		}
		assert (this.header == null) : "header may only be set once";
		this.header = header;
	}

	public void setFooter(Footer footer) {
		if (footer == null) {
			return;
		}
		assert (this.footer == null) : "footer may only be set once";
		this.footer = footer;
		StyleUtils.addStyle(footer, STYLE_FOOTER);
	}

	public void show() {
		ensureDismissButton();
		redraw();
		visible = true;
		Modal.MODAL_BACKDROP.show();
		getElement().getStyle().setDisplay(Display.BLOCK);
		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				StyleUtils.addStyle(Modal.this, STYLE_VISIBLE);
				return false;
			}
		}, 150);

	}

	public void hide() {
		visible = false;
		StyleUtils.removeStyle(Modal.this, STYLE_VISIBLE);
		Modal.MODAL_BACKDROP.hide();
		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				getElement().getStyle().clearDisplay();
				return false;
			}
		}, 150);
	}

	public void toggleVisibility() {
		if (visible) {
			hide();
		}
		else {
			show();
		}
	}

	@Override
	public Widget getWidget() {
		return widget;
	}

	@Override
	public void setWidget(IsWidget w) {
		setWidget(w.asWidget());
	}

	@Override
	public void setWidget(Widget w) {
		widget = w;
		bodyContainer.append(widget);
	}

	@Override
	public void setTitle(String title) {
		this.title = title;
		super.setTitle(title);
	}

	@Override
	public String getTitle() {
		return title;
	}

	public void setDismissable(boolean dismissable) {
		this.dismissable = dismissable;
	}

	@Override
	public void redraw() {
		headerContainer.clear();

		if (dismissable) {
			headerContainer.append(ensureDismissButton());
		}
		if (header != null) {
			headerContainer.append(header);
		}
		else if (title != null) {
			Heading titleHeading = new Heading(4);
			titleHeading.setText(title);
			headerContainer.append(titleHeading);
		}

		if (headerContainer.getWidgetCount() > 0) {
			StyleUtils.addStyle(headerContainer, STYLE_HEADER);
		}

		contentContainer.clear();
		contentContainer.append(headerContainer);
		contentContainer.append(bodyContainer);
		contentContainer.append(footer);
	}

	private Anchor<?> ensureDismissButton() {
		if (dismissable && dismissButton == null) {
			dismissButton = new Anchor("&times;");
			StyleUtils.addStyle(dismissButton, STYLE_CLOSE);
			dismissButton.addClickHandler(new ClickHandler() {

				@Override
				public void onClick(ClickEvent event) {
					hide();
				}

			});
		}
		return dismissButton;
	}
}
