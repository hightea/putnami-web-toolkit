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

import java.util.Map;

import com.google.common.collect.Maps;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.TableElement;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.model.client.base.HasReadonly;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.event.RowClickEvent;
import fr.putnami.pwt.core.widget.client.event.RowClickEvent.Handler;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class Table<T> extends AbstractPanel implements
HasReadonly,
CloneableWidget,
RowClickEvent.HasRowClickHandlers {

	private static final CssStyle STYLE_TABLE = new SimpleStyle("table");
	private static final CssStyle STYLE_RESPONSIVE = new SimpleStyle("table-responsive");
	private static final CssStyle STYLE_STRIPED = new SimpleStyle("table-striped");
	private static final CssStyle STYLE_HOVER = new SimpleStyle("table-hover");
	private static final CssStyle STYLE_CONDENSED = new SimpleStyle("table-condensed");
	private static final CssStyle STYLE_BORDERED = new SimpleStyle("table-bordered");

	public static final String BODY_DEFAULT = "DEFAULT_BODY";

	private final Container tableContainer = new Container(TableElement.TAG);
	private TableHead head;
	private Map<String, TableBody<T>> bodies = Maps.newHashMap();;

	private Boolean readonly;

	private boolean striped = true;
	private boolean hover = true;
	private boolean condensed = false;
	private boolean bordered = false;

	public Table() {
		super(DivElement.TAG);

		endConstruct();

		setStriped(striped);
		setHover(hover);
		setCondensed(condensed);
	}

	protected Table(Table<T> source) {
		super(source);

		endConstruct();

		setStriped(source.striped);
		setHover(source.hover);
		setCondensed(source.condensed);
		setBordered(source.bordered);

		setReadonly(source.readonly);

		setHead(WidgetUtils.cloneWidget(source.head));
		for (TableBody<T> body : source.bodies.values()) {
			addBody(WidgetUtils.cloneWidget(body));
		}
	}

	@Override
	public IsWidget cloneWidget() {
		return new Table<T>(this);
	}

	private void endConstruct() {
		append(tableContainer);

		StyleUtils.addStyle(this, STYLE_RESPONSIVE);
		StyleUtils.addStyle(tableContainer, STYLE_TABLE);
	}

	public boolean isStriped() {
		return striped;
	}

	public void setStriped(boolean striped) {
		this.striped = striped;
		StyleUtils.toggleStyle(tableContainer, STYLE_STRIPED, striped);
	}

	public boolean isCondensed() {
		return condensed;
	}

	public void setCondensed(boolean condensed) {
		this.condensed = condensed;
		StyleUtils.toggleStyle(tableContainer, STYLE_CONDENSED, condensed);
	}

	public boolean isHover() {
		return hover;
	}

	public void setHover(boolean hover) {
		this.hover = hover;
		StyleUtils.toggleStyle(tableContainer, STYLE_HOVER, hover);
	}

	public boolean isBordered() {
		return bordered;
	}

	public void setBordered(boolean bordered) {
		this.bordered = bordered;
		StyleUtils.toggleStyle(tableContainer, STYLE_BORDERED, bordered);
	}

	@Override
	public Boolean getReadonly() {
		return readonly;
	}

	@Override
	public void setReadonly(Boolean readonly) {
		this.readonly = readonly;
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof TableHead) {
			this.setHead((TableHead) w);
		}
		if (w instanceof TableRow) {
			this.addRow((TableRow<T>) w);
		}
		if (w instanceof TableBody) {
			this.addBody((TableBody<T>) w);
		}
	}

	@Override
	public HandlerRegistration addRowClickHandler(Handler handler) {
		return getDefaultBody().addRowClickHandler(handler);
	}

	public TableBody<T> getBody(String bodyId) {
		TableBody<T> body = this.bodies.get(bodyId);
		if (body == null) {
			body = createBody(bodyId);
			this.addBody(body);
		}
		return body;
	}

	private void setHead(TableHead head) {
		this.head = head;
		tableContainer.append(this.head);
	}

	private void addRow(TableRow<T> row) {
		getDefaultBody().addRow(row);
	}

	protected TableBody<T> getDefaultBody() {
		return getBody(Table.BODY_DEFAULT);
	}

	protected TableBody<T> createBody(String bodyId) {
		TableBody<T> body = new TableBody<T>(bodyId);
		body.setReadonly(readonly);
		return body;
	}

	protected void addBody(TableBody<T> body) {
		bodies.put(body.getBodyId(), body);
		tableContainer.append(body);
	}

	protected TableHead ensureTableHead() {
		if (this.head == null) {
			TableHead head = new TableHead();
			this.setHead(head);
		}
		return this.head;
	}

	public TableHead getTableHeader() {
		return ensureTableHead();
	}

}
