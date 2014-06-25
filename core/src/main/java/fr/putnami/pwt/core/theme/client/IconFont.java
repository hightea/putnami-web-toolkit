package fr.putnami.pwt.core.theme.client;

import java.util.Map;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import fr.putnami.pwt.core.widget.client.base.SimpleStyle;

public abstract class IconFont extends CssLink {

	public static final String ICON_CALENDAR = "calendar";
	public static final String ICON_ADD = "plus";
	public static final String ICON_MENU = "menu";
	public static final String ICON_MAIL = "mail";
	public static final String ICON_DRAG = "drag";
	public static final String ICON_SORT = "sort";
	public static final String ICON_SORT_UP = "sort-up";
	public static final String ICON_SORT_DOWN = "sort-down";
	public static final String ICON_FILTER = "filter";
	public static final String ICON_ANGLE_DOUBLE_RIGHT = "angle-double-right";
	public static final String ICON_ANGLE_DOUBLE_DOWN = "angle-double-down";

	private final String preffix;
	private final Map<String, String> aliases = Maps.newHashMap();

	public IconFont(String href, String preffix) {
		super(href, -1);
		this.preffix = preffix == null ? "" : preffix;
	}

	protected void addAlias(String alias, String target) {
		aliases.put(alias, target);
	}

	public CssStyle getStyle(String iconName) {
		if (Strings.isNullOrEmpty(iconName)) {
			return SimpleStyle.EMPTY_STYLE;
		}
		String className = transformClassName(iconName);
		if (aliases.containsKey(className)) {
			className = aliases.get(className);
		}
		return new SimpleStyle(this.preffix + className);
	}

	protected String transformClassName(String className) {
		return className;
	}

}
