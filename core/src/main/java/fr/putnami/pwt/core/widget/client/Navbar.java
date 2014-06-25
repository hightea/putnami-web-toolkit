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

import com.google.gwt.dom.client.AnchorElement;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.uibinder.client.UiChild;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.mvp.client.event.StartActivityEvent;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.helper.CollapseHelper;
import fr.putnami.pwt.core.widget.client.util.AnchorUtils;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class Navbar extends AbstractPanel implements
CloneableWidget {

	private static final CssStyle STYLE_NAVBAR = new SimpleStyle("navbar");
	private static final CssStyle STYLE_BRAND = new SimpleStyle("navbar-brand");
	private static final CssStyle STYLE_NAV = new SimpleStyle("nav navbar-nav");
	private static final CssStyle STYLE_HEADER = new SimpleStyle("navbar-header");
	private static final CssStyle STYLE_TOGGLE = new SimpleStyle("navbar-toggle");
	private static final CssStyle STYLE_NAV_BUTTON = new SimpleStyle("navbar-btn");
	private static final CssStyle STYLE_NAVBAR_LEFT = new SimpleStyle("navbar-left");
	private static final CssStyle STYLE_NAVBAR_RIGHT = new SimpleStyle("navbar-right");
	private static final CssStyle STYLE_NAVBAR_TEXT = new SimpleStyle("navbar-text");
	private static final CssStyle STYLE_NAVBAR_LINK = new SimpleStyle("navbar-link");
	private static final CssStyle STYLE_COLLAPSE = new SimpleStyle("navbar-collapse");
	private static final CssStyle STYLE_TEXT_MUTED = new SimpleStyle("text-muted");
	private static final CssStyle STYLE_FLUID = new SimpleStyle("container-fluid");

	public enum Type implements CssStyle {

		DEFAULT("navbar-default"),
		INVERSE("navbar-inverse");

		private final String style;

		private Type(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return this.style;
		}

	}

	public enum Position implements CssStyle {
		DEFAULT(null),
		FIXED_TOP("navbar-fixed-top"),
		FIXED_BOTTOM("navbar-fixed-bottom"),
		STATIC_TOP("navbar-static-top");

		private final String style;

		private Position(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return this.style;
		}
	}

	private class NavbarButton extends Widget implements CloneableWidget {

		public NavbarButton() {
			setElement(Document.get().createAnchorElement());
			StyleUtils.addStyle(this, STYLE_TOGGLE);
			StyleUtils.addStyle(this, STYLE_TEXT_MUTED);
			AnchorElement.as(getElement()).setHref(AnchorUtils.DUMMY_HREF);
			getElement().appendChild(createIcon());
		}

		@Override
		public IsWidget cloneWidget() {
			return new NavbarButton();
		}

		public void setCollapseContainer(Container collapseContainer) {
			collapseHelper = CollapseHelper.apply(this, collapseContainer.getElement(), true);
		}

		private Element createIcon() {
			Icon icon = new Icon();
			icon.setType(IconFont.ICON_MENU);
			return icon.getElement();
		}
	}

	private final Container headerContainer = new Container();
	private final Container collapseContainer = new Container();

	private Type type = Type.DEFAULT;
	private Position position = Position.DEFAULT;
	private CollapseHelper collapseHelper;

	private StartActivityEvent.Handler collapseHandler = new StartActivityEvent.Handler() {

		@Override
		public void onStartActivity(StartActivityEvent event) {
			if (collapseHelper != null) {
				collapseHelper.collapse();
			}
		}
	};

	public Navbar() {
		super(DivElement.TAG);
		endConstruct();
	}

	protected Navbar(Navbar source) {
		super(source);
		this.type = source.type;
		this.position = source.position;
		for (Widget collapseWidget : source.collapseContainer) {
			collapseContainer.append(WidgetUtils.cloneWidget(collapseWidget));
		}
		endConstruct();
	}

	private void endConstruct() {
		Container contentContainer = new Container();
		append(contentContainer);
		contentContainer.append(headerContainer);
		contentContainer.append(collapseContainer);

		NavbarButton button = new NavbarButton();
		button.setCollapseContainer(collapseContainer);
		headerContainer.append(button);

		StyleUtils.addStyle(this, STYLE_NAVBAR);
		StyleUtils.addStyle(contentContainer, STYLE_FLUID);
		StyleUtils.addStyle(headerContainer, STYLE_HEADER);
		StyleUtils.addStyle(collapseContainer, STYLE_COLLAPSE);

		setType(type);
		setPosition(position);

		MvpController.get().addStartActivityHandler(collapseHandler);
	}

	@Override
	public IsWidget cloneWidget() {
		return new Navbar(this);
	}

	@UiChild(tagname = "Brand")
	public void addBrand(Anchor w) {
		StyleUtils.addStyle(asWidgetOrNull(w), STYLE_BRAND);
		headerContainer.add(w);
	}

	@UiChild(tagname = "left")
	public void addLeft(IsWidget w) {
		StyleUtils.addStyle(asWidgetOrNull(w), STYLE_NAVBAR_LEFT);
		add(w);
	}

	@UiChild(tagname = "right")
	public void addRight(IsWidget w) {
		StyleUtils.addStyle(asWidgetOrNull(w), STYLE_NAVBAR_RIGHT);
		add(w);
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof Nav) {
			updateNavStyle((Nav) w);
		}
		if (w instanceof Button || w instanceof ButtonGroup) {
			StyleUtils.addStyle((Widget) w, STYLE_NAV_BUTTON);
		}
		collapseContainer.append(w);
	}

	public void setType(Type type) {
		this.type = type;
		StyleUtils.addStyle(getElement(), this.type);
	}

	public void setPosition(Position position) {
		this.position = position;
		StyleUtils.addStyle(getElement(), this.position);
	}

	private void updateNavStyle(Nav nav) {
		StyleUtils.addStyle(nav, STYLE_NAV);
		nav.setStyle(null);
	}
}
